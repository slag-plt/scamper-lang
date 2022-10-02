import { asObj_, entry, Env, ecall, eif, elam, elet, epair, isValue, Exp, Stmt, sexp, expToString, sbinding, svalue, serror, Name, econd, nlecond, eand, eor, nlebool, nleand, nleor, simported, nleprim, sdefine, Prim, isObjKind, EnvEntry, nlestruct, asStruct_, isStructKind
, LetKind, 
nlecall,
isString,
isBoolean,
asBool_,
asString_,
stestcase,
stestresult} from './lang.js'
import { Result, error, join, ok, rethrow, errorDetails, ICE } from './result.js'
import { msg } from './messages.js'
import { preludeEnv } from './lib/prelude.js'
import { imageLib } from './lib/image.js'
import { musicLib } from './lib/music.js'

function runtimeError <T> (message: string, s?: Exp, hint?: string): Result<T> {
  return s
    ? error(msg('phase-runtime'), message, s.range, expToString(s), hint)
    : error(msg('phase-runtime'), message, undefined, undefined, hint)
}

function namesInclude (names: Name[], x: string): boolean {
  return names.some(n => n.value === x)
}

function substitute (e1: Exp, x: string, e2: Exp): Exp {
  switch (e2.tag) {
    case 'var':
      return e2.value === x ? e1 : e2
    case 'lit':
      return e2
    case 'call':
      return ecall(e2.range, substitute(e1, x, e2.head), e2.args.map((e) => substitute(e1, x, e)))
    case 'lam':
      return namesInclude(e2.args, x) ? e2 : elam(e2.range, e2.args, substitute(e1, x, e2.body))
    case 'if':
      return eif(e2.range, substitute(e1, x, e2.e1), substitute(e1, x, e2.e2), substitute(e1, x, e2.e3))
    case 'nil':
      return e2
    case 'pair':
      return epair(e2.range, substitute(e1, x, e2.e1), substitute(e1, x, e2.e2))
    case 'let':
      return elet(
        e2.range,
        e2.kind,
        substituteInBindings(e1, x, e2.bindings, e2.kind),
        // N.B., we only substitute into the body if we didn't shadow x via one
        // of our bindings. Note that this won't occur in the 'let' case.
        // But it occurs in the other cases as long as one of the bindings
        // mentions x.
        e2.kind !== 'let' && inBindings(x, e2.bindings) ? e2.body : substitute(e1, x, e2.body)
      )
    case 'cond':
      return econd(e2.range, e2.branches.map(b => [substitute(e1, x, b[0]), substitute(e1, x, b[1])]))
    case 'and':
      return eand(e2.range, e2.args.map(e => substitute(e1, x, e)))
    case 'or':
      return eor(e2.range, e2.args.map(e => substitute(e1, x, e)))
    case 'struct':
      return e2
    case 'obj':
      return e2
    case 'prim':
      return e2
  }
}

function inBindings (x: string, bindings: [Name, Exp][]): boolean {
  for (let i = 0; i < bindings.length; i++) {
    if (bindings[i][0].value === x) {
      return true
    }
  }
  return false
}

function substituteInBindings (e1: Exp, x: string, bindings: [Name, Exp][], kind: LetKind): [Name, Exp][] {
  const result = new Array(bindings.length)
  let seenVar = false
  for (let i = 0; i < bindings.length; i++) {
    const [y, body] = bindings[i]
    // For a 'let', shadowing is local to each binding.
    if (kind === 'let') {
      result[i] = [y, x === y.value ? body : substitute(e1, x, body)]

    // For a 'let' and 'letrec', shadowing telescopes from previous bindings.
    } else {
      if (seenVar) {
        result[i] = [y, body]
      } else if (y.value === x) {
        // N.B., in the let* case, y is not visible in the body of this binding,
        // but visible in _subsequent_ bindings. In the letrec case, the value
        // is immediately visible so it is shadowed.
        result[i] = kind === 'let*' ? [y, substitute(e1, x, body)] : [y, body]
        seenVar = true
      } else {
        result[i] = [y, substitute(e1, x, body)]
      }
    }
  }
  return result
}

function substituteAll (es: Exp[], xs: Name[], e: Exp) {
  for (let i = 0; i < es.length; i++) {
    e = substitute(es[i], xs[i].value, e)
  }
  return e
}

function substituteIfFreeVar (env: Env, e: Exp): Result<Exp> {
  switch (e.tag) {
    case 'var':
      // N.B., repeatedly substitute if a free var is bound to another
      // free var. Kind of gross, probably should re think it.
      if (env.has(e.value)) {
        const result = env.get(e.value)!.value
        if (result.tag === 'var') {
          return substituteIfFreeVar(env, result)
        } else {
          return ok(result)
        }
      } else {
        return runtimeError(msg('error-var-undef', e.value), e)
      }
    default:
      return ok(e)
  }
}

async function stepExp (env: Env, e: Exp): Promise<Result<Exp>> {
  switch (e.tag) {
    // N.B., at this point, variables should be free variables bound in the
    // environment. We rely on sprinkled calls to substituteWithEnv once an
    // expression is a value in order to resolve free variables before they
    // are used.
    case 'var':
      return ok(e)
    case 'lit':
      return ok(e)
    case 'call':
      // NOTE: we allow variables in head position so that (f x) works
      // where f is a top-level binding. If we change the infrastructure
      // of variables and substitution, we'll need to revisit this choice.
      if (!isValue(e.head)) {
        return (await stepExp(env, e.head)).asyncAndThen(async headp => await stepExp(env, ecall(e.range, headp, e.args)))
      } else {
        // Try to stepExp one of the call's arguments
        for (let i = 0; i < e.args.length; i++) {
          if (!isValue(e.args[i])) {
            const argsp = [...e.args]
            return (await stepExp(env, argsp[i])).andThen(eip => {
              argsp[i] = eip
              return ok(ecall(e.range, e.head, argsp))
            })
          }
        }
        // If we did not stepExp any arguments, then evaluate the full call.
        // First, substitute away free variables before resolving the call.
        return substituteIfFreeVar(env, e.head).asyncAndThen(head =>
          join(e.args.map(x => substituteIfFreeVar(env, x))).asyncAndThen(async (args: Exp[]): Promise<Result<Exp>> => {
            switch (head.tag) {
              case 'lam':
                if (args.length === head.args.length) {
                  return ok(substituteAll(args, head.args, head.body))
                } else {
                  return runtimeError(msg('error-arity', 'lambda', head.args.length, args.length), e)
                }
              case 'prim':
                return await head.prim(env, args, e)
              default:
                return runtimeError(msg('error-type-expected-call', e.head.tag), e)
            }
          }))
      }
    case 'lam':
      return ok(e)
    case 'if':
      if (!isValue(e.e1)) {
        return (await stepExp(env, e.e1)).andThen(e1p => ok(eif(e.range, e1p, e.e2, e.e3)))
      } else {
        return substituteIfFreeVar(env, e.e1).andThen(e1 => {
          switch (e1.tag) {
            case 'lit':
              if (e1.value.tag === 'bool') {
                return e1.value.value ? ok(e.e2) : ok(e.e3)
              } else {
                return runtimeError(msg('error-type-expected-cond', e1.value.tag), e)
              }
            default:
              return runtimeError(msg('error-type-expected-cond', e1.tag), e)
          }
        })
      }
    case 'nil':
      return ok(e)
    case 'pair':
      if (!isValue(e.e1)) {
        return (await stepExp(env, e.e1)).andThen(e1p => ok(epair(e.range, e1p, e.e2)))
      } else if (!isValue(e.e2)) {
        return (await stepExp(env, e.e2)).andThen(e2p => ok(epair(e.range, e.e1, e2p)))
      } else {
        return ok(e)
      }
    case 'let':
      if (e.bindings.length > 0) {
        const x = e.bindings[0][0]
        const e1 = e.bindings[0][1]
        if (!isValue(e1)) {
          return (await stepExp(env, e1)).andThen(e1p => {
            const bindings = [...e.bindings]
            bindings[0] = [x, e1p]
            return ok(elet(e.range, e.kind, bindings, e.body))
          })
        } else if (e.bindings.length === 1) {
          return ok(substitute(e1, x.value, e.body))
        } else if (e.kind === 'let') {
          return ok(elet(
            e.range,
            e.kind,
            e.bindings.slice(1),
            substitute(e1, x.value, e.body)
          ))
        } else if (e.kind === 'let*') {
          return ok(elet(
            e.range,
            e.kind,
            substituteInBindings(e1, x.value, e.bindings.slice(1), e.kind),
            inBindings(x.value, e.bindings.slice(1)) ? e.body : substitute(e1, x.value, e.body)
          ))
        } else {
          // TODO: the semantics we want... I think... is:
          // 
          // let recbind = { {letrec ([f (lambda (n) e)] f) / f} (lambda (n) e) / f }
          //
          //     (letrec ([f (lambda (n) e)] ... bindings) body)
          // --> (letrec (recbind bindings) (recbind body))
          //
          // i.e., a version of f that carries an "unwrapping" package achieved via letrec.
          // Note that:
          //   (a) this only works for lambdas. If f mentions itself in its definition and f is
          //   not a lambda, then we go into an infinite loop. For simplicity's sake, we should
          //   allow for the body of the binding to evaluate to a value naturally without binding
          //   its recursive occurrence.
          //   (b) this... should? work for mutually recursive bindings by playing the same
          //   trick for all recursive bindings mentioned in this one. Perhaps we need to evaluate
          //   specially in this case, identifying which bindings are (mutually) recursive and
          //   which are not.
          throw new ICE('stepExp', 'letrec not yet implemented')
        }
      } else {
        return ok(e.body)
      }
    case 'cond':
      if (e.branches.length === 0) {
        return runtimeError(msg('error-cond-no-branch-applies'), e)
      } else {
        const guard = e.branches[0][0]
        const body = e.branches[0][1]
        return substituteIfFreeVar(env, guard).asyncAndThen(async guard => {
          if (isValue(guard)) {
            if (guard.tag === 'lit' && guard.value.tag === 'bool' && guard.value.value === true) {
              return ok(body)
            } else if (guard.tag === 'lit' && guard.value.tag === 'bool' && guard.value.value === false) {
              return ok(nlecond([...e.branches.slice(1)]))
            } else {
              return runtimeError(msg('error-type-expected-cond', guard.tag), e)
            }
          } else {
            return (await stepExp(env, guard)).andThen(guardp =>
              ok(nlecond([[guardp, body], ...e.branches.slice(1)])))
          }
        })
      }
    case 'and':
      if (e.args.length === 0) {
        return ok(nlebool(true))
      } else {
        const head = e.args[0]
        if (isValue(head)) {
          if (head.tag === 'lit' && head.value.tag === 'bool') {
            return head.value.value
              ? ok(nleand([...e.args.slice(1)]))
              : ok(nlebool(false))
          } else {
            return runtimeError(msg('error-type-expected', 'bool', head.tag), e)
          }
        } else {
          return (await stepExp(env, head)).andThen(headp =>
            ok(nleand([headp, ...e.args.slice(1)])))
        }
      }
    // N.B., or is identical to and expect dualized---factor redundancy?
    case 'or':
      if (e.args.length === 0) {
        return ok(nlebool(false))
      } else {
        const head = e.args[0]
        if (isValue(head)) {
          if (head.tag === 'lit' && head.value.tag === 'bool') {
            return !head.value.value
              ? ok(nleor([...e.args.slice(1)]))
              : ok(nlebool(true))
          } else {
            return runtimeError(msg('error-type-expected', 'bool', head.tag), e)
          }
        } else {
          return (await stepExp(env, head)).andThen(headp =>
            ok(nleor([headp, ...e.args.slice(1)])))
        }
      }
    case 'struct':
      return ok(e)
    case 'obj':
      return ok(e)
    case 'prim':
      return ok(e)
  }
}

async function evaluateExp (env: Env, e:Exp) : Promise<Result<Exp>> {
  while (!isValue(e)) {
    // TODO: need to check whether e == stepExp(e), i.e., we're in a loop
    const result = await stepExp(env, e)
    switch (result.tag) {
      case 'error':
        return rethrow(result)
      case 'ok':
        e = result.value
    }
  }
  return ok(e)
}

function resultToStmt (result: Result<Stmt>): Stmt {
  switch (result.tag) {
    case 'error':
      return serror(result.details)
    case 'ok':
      return result.value
  }
}

async function stepStmt (env: Env, s: Stmt): Promise<[Env, Stmt]> {
  switch (s.tag) {
    case 'error':
      return [env, s]
    case 'binding':
      return [env, s]
    case 'testresult':
      return [env, s]
    case 'value':
      return [env, s]
    case 'imported':
      return [env, s]
    case 'define':
      if (isValue(s.value)) {
        return [
          env.append(s.name.value, entry(s.value, 'binding', s.name.range)),
          sbinding(s.name.value)
        ]
      } else {
        return [
          env,
          resultToStmt((await stepExp(env, s.value)).andThen(e => ok(sdefine(s.name, e))))
        ]
      }
    case 'struct': {
      const name = s.id.value
      const predName = `${name}?`
      // primitive for type-testing predicate: id?
      const predPrim: Prim = async (env, args, app) =>
        args.length !== 1
          ? runtimeError(msg('error-arity', predName, 1, args.length), app)
          : ok(nlebool(isStructKind(args[0], name)))
      // field-accessing primitives: id-field?
      const fieldPrims: [string, EnvEntry][] = s.fields.map(f => {
        const fieldName = `${name}-${f.value}`
        return [fieldName, entry(
          nleprim(async (env, args, app) =>
            args.length !== 1
              ? runtimeError(msg('error-arity', fieldName, 1, args.length), app)
              : !isStructKind(args[0], name)
                ? runtimeError(msg('error-type-expected-fun', 1, fieldName, `struct ${name}`, args[0].tag))
                : ok((asStruct_(args[0]) as any)[f.value] as Exp)),
          `struct ${name}`,
          f.range
        )]
      })
      // constructor primitive: id
      const ctorPrim: Prim = async (env, args, app) => {
        if (args.length !== s.fields.length) {
          return runtimeError(msg('error-arity', name, s.fields.length, args.length), app)
        } else {
          const obj: any = { }
          s.fields.forEach((f, i) => obj[f.value] = args[i])
          return ok(nlestruct(name, obj))
        }
      }
      return [
        env.concat(new Env([
          [name, entry(nleprim(ctorPrim), 'struct ${name}', s.id.range)],
          [predName, entry(nleprim(predPrim), 'struct ${name}', s.id.range)],
          ...fieldPrims
        ])),
        sbinding(`struct ${name}`)
      ]
    }
    case 'testcase': {
      const result: Result<Stmt> = await (await evaluateExp(env, s.desc)).asyncAndThen(async e1 =>
        (await evaluateExp(env, s.expected)).asyncAndThen(async expected =>
          (await evaluateExp(env, s.actual)).asyncAndThen(async actual =>
            (await evaluateExp(env, nlecall(s.comp, [expected, actual]))).andThen(e2 => {
              if (!isString(e1)) {
                return runtimeError(msg('error-type-expected', 'string', e1.tag), s.desc)
              } else if (!isBoolean(e2)) {
                return runtimeError(msg('error-type-expected', 'bool', e2.tag), s.comp)
              }
              const desc = asString_(e1)
              const passed = asBool_(e2)
              return ok(passed
                ? stestresult(desc, true)
                : stestresult(desc, false, undefined, expected, actual))
            }))))
      if (result.tag === 'error') {
        return [env, serror(result.details)]
      } else {
        return [env, result.value]
      }
    }
    // N.B., as a last step, substitute free variables away when they are values.
    case 'exp': {
      const result = isValue(s.value)
        ? resultToStmt(substituteIfFreeVar(env, s.value).andThen(vp => ok(svalue(vp))))
        : resultToStmt((await stepExp(env, s.value)).andThen(v =>
            isValue(v)
              ? substituteIfFreeVar(env, v).andThen(vp => ok(svalue(vp)))
              : ok(sexp(v)) as Result<Stmt>))
      return [env, result]
    }
    case 'import':
      if (internalLibs.has(s.source)) {
        return [
          new Env([...env.items(), ...internalLibs.get(s.source)!.items()]),
          simported(s.source)
        ]
      } else {
        return [
          env,
          serror([errorDetails(
            msg('phase-runtime'),
            msg('error-import-not-found', s.source, s.range)
          )])
        ]
      }
  }
}

const internalLibs: Map<string, Env> = new Map([
  ['image', imageLib],
  ['music', musicLib]
])

export {
  runtimeError, substitute,
  stepExp, stepStmt, evaluateExp,
  preludeEnv, internalLibs
}
