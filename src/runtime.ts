/* eslint-disable @typescript-eslint/no-unsafe-member-access */
import * as L from './lang.js'
import { Result, error, join, ok, rethrow, errorDetails, ICE } from './result.js'
import { msg } from './messages.js'
import { preludeEnv } from './lib/prelude.js'
import { imageLib } from './lib/image.js'
import { musicLib } from './lib/music.js'

function runtimeError <T> (message: string, s?: L.Exp, hint?: string): Result<T> {
  return s
    ? error(msg('phase-runtime'), message, s.range, L.expToString(s), hint)
    : error(msg('phase-runtime'), message, undefined, undefined, hint)
}

function namesInclude (names: L.Name[], x: string): boolean {
  return names.some(n => n.value === x)
}

function substitute (v: L.Value, x: string, e: L.Exp): L.Exp {
  switch (e.tag) {
    case 'value':
      return e
    case 'var':
      return e.value === x ? L.nlevalue(v) : e
    case 'lit':
      return e
    case 'call':
      return L.ecall(e.range, substitute(v, x, e.head), e.args.map((e) => substitute(v, x, e)))
    case 'lam':
      return namesInclude(e.args, x) ? e : L.elam(e.range, e.args, substitute(v, x, e.body))
    case 'if':
      return L.eif(e.range, substitute(v, x, e.e1), substitute(v, x, e.e2), substitute(v, x, e.e3))
    case 'nil':
      return e
    case 'pair':
      return L.epair(e.range, substitute(v, x, e.e1), substitute(v, x, e.e2))
    case 'let':
      return L.elet(
        e.range,
        e.kind,
        substituteInBindings(v, x, e.bindings, e.kind),
        // N.B., we only substitute into the body if we didn't shadow x via one
        // of our bindings. Note that this won't occur in the 'let' case.
        // But it occurs in the other cases as long as one of the bindings
        // mentions x.
        e.kind !== 'let' && inBindings(x, e.bindings) ? e.body : substitute(v, x, e.body)
      )
    case 'cond':
      return L.econd(e.range, e.branches.map(b => [substitute(v, x, b[0]), substitute(v, x, b[1])]))
    case 'and':
      return L.eand(e.range, e.args.map(e => substitute(v, x, e)))
    case 'or':
      return L.eor(e.range, e.args.map(e => substitute(v, x, e)))
    case 'match':
      return L.ematch(e.range,
        substitute(v, x, e.scrutinee),
        e.branches.map(b => [b[0], substitute(v, x, b[1])]),
        e.bracket)
  }
}

function inBindings (x: string, bindings: [L.Name, L.Exp][]): boolean {
  for (let i = 0; i < bindings.length; i++) {
    if (bindings[i][0].value === x) {
      return true
    }
  }
  return false
}

function substituteInBindings (v: L.Value, x: string, bindings: [L.Name, L.Exp][], kind: L.LetKind): [L.Name, L.Exp][] {
  const result = new Array<[L.Name, L.Exp]>(bindings.length)
  let seenVar = false
  for (let i = 0; i < bindings.length; i++) {
    const [y, body] = bindings[i]
    // For a 'let', shadowing is local to each binding.
    if (kind === 'let') {
      result[i] = [y, x === y.value ? body : substitute(v, x, body)]

    // For a 'let' and 'letrec', shadowing telescopes from previous bindings.
    } else {
      if (seenVar) {
        result[i] = [y, body]
      } else if (y.value === x) {
        // N.B., in the let* case, y is not visible in the body of this binding,
        // but visible in _subsequent_ bindings. In the letrec case, the value
        // is immediately visible so it is shadowed.
        result[i] = kind === 'let*' ? [y, substitute(v, x, body)] : [y, body]
        seenVar = true
      } else {
        result[i] = [y, substitute(v, x, body)]
      }
    }
  }
  return result
}

function substituteAll (es: L.Exp[], xs: L.Name[], e: L.Exp) {
  for (let i = 0; i < es.length; i++) {
    e = substitute(es[i], xs[i].value, e)
  }
  return e
}

function substituteEnv (env: L.Env, e: L.Exp) {
  const bindings = [...env.entries.entries()]
  for (let i = 0; i < bindings.length; i++) {
    e = substitute(bindings[i][1].value, bindings[i][0], e)
  }
  return e
}

function substituteIfFreeVar (env: L.Env, e: L.Exp): Result<L.Exp> {
  switch (e.tag) {
    case 'var':
      // N.B., repeatedly substitute if a free var is bound to another
      // free var. Kind of gross, probably should re think it.
      if (env.has(e.value)) {
        return ok(L.nlevalue(env.get(e.value)!.value))
      } else {
        return runtimeError(msg('error-var-undef', e.value), e)
      }
    default:
      return ok(e)
  }
}

function tryMatch (v: L.Value, p: L.Pat): L.Env | undefined {
  if (p.tag === 'wild') {
    return new L.Env([])
  } else if (p.tag === 'null' && L.valueIsNull(v)) {
    return new L.Env([])
  } else if (p.tag === 'var') {
    return new L.Env([[p.id, L.entry(v, 'match')]])
  } else if (p.tag === 'lit' && p.lit.tag === 'bool' && p.lit.value === v) {
    return new L.Env([])
  } else if (p.tag === 'lit' && p.lit.tag === 'num' && p.lit.value === v) {
    return new L.Env([])
  } else if (p.tag === 'lit' && p.lit.tag === 'str' && p.lit.value === v) {
    return new L.Env([])
  } else if (p.tag === 'lit' && p.lit.tag === 'char' && L.valueIsChar(v) && p.lit.value === v.value) {
    return new L.Env([])
  } else if (p.tag === 'ctor' && (L.valueIsPair(v) || L.valueIsStruct(v))) {
    const head = p.head
    const args = p.args
    // Special cases: pairs are matched with either pair or cons
    if ((head === 'pair' || head === 'cons') && args.length === 2 && L.valueIsPair(v)) {
      const env1 = tryMatch(v.fst, args[0])
      const env2 = tryMatch(v.snd, args[1])
      return env1 && env2 ? env1.concat(env2) : undefined
    } else if (L.valueIsStructKind(v, head)) {
      const fields = [...(v.fields as Map<string, L.Value>).values()]
      if (fields.length === args.length) {
        let env = new L.Env([])
        for (let i = 0; i < fields.length; i++) {
          const env2 = tryMatch(fields[i], args[i])
          if (!env2) {
            return undefined
          }
          env = env.concat(env2)
        }
        return env
      } else {
        // TODO: should we throw an error here?
        return undefined
      }
    } else {
      // N.B., at this point, we have run out of options, so the pattern match
      // has failed.
      return undefined
    }
  }
}

async function stepExp (env: L.Env, e: L.Exp): Promise<Result<L.Exp>> {
  switch (e.tag) {
    case 'value':
      return ok(e)
    // N.B., at this point, variables should be free variables bound in the
    // environment. We rely on sprinkled calls to substituteWithEnv once an
    // expression is a value in order to resolve free variables before they
    // are used.
    case 'var':
      return ok(e)
    case 'lit':
      return ok(L.litToValue(e.value))
    case 'call':
      // NOTE: we allow variables in head position so that (f x) works
      // where f is a top-level binding. If we change the infrastructure
      // of variables and substitution, we'll need to revisit this choice.
      if (!L.isValue(e.head)) {
        return (await stepExp(env, e.head)).asyncAndThen(async headp => await stepExp(env, L.ecall(e.range, headp, e.args)))
      } else {
        // Try to stepExp one of the call's arguments
        for (let i = 0; i < e.args.length; i++) {
          if (!L.isValue(e.args[i])) {
            const argsp = [...e.args]
            return (await stepExp(env, argsp[i])).andThen(eip => {
              argsp[i] = eip
              return ok(L.ecall(e.range, e.head, argsp))
            })
          }
        }
        // If we did not stepExp any arguments, then evaluate the full call.
        // First, substitute away free variables before resolving the call.
        return substituteIfFreeVar(env, e.head).asyncAndThen(head =>
          join(e.args.map(x => substituteIfFreeVar(env, x))).asyncAndThen(async (args: L.Exp[]): Promise<Result<L.Exp>> => {
            if (head.tag === 'value') {
              if (L.valueIsLambda(head)) {

              } else if (L.valueIsPrim(head)) {

              } else {
                return runtimeError(msg('error-type-expected-call', e.head.tag), e)
              }
            } else {
              throw new ICE('stepExp', 'call head is not a value')
            }
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
      if (!L.isValue(e.e1)) {
        return (await stepExp(env, e.e1)).andThen(e1p => ok(L.eif(e.range, e1p, e.e2, e.e3)))
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
      if (!L.isValue(e.e1)) {
        return (await stepExp(env, e.e1)).andThen(e1p => ok(L.epair(e.range, e1p, e.e2)))
      } else if (!L.isValue(e.e2)) {
        return (await stepExp(env, e.e2)).andThen(e2p => ok(L.epair(e.range, e.e1, e2p)))
      } else {
        return ok(e)
      }
    case 'let':
      if (e.bindings.length > 0) {
        const x = e.bindings[0][0]
        const e1 = e.bindings[0][1]
        if (!L.isValue(e1)) {
          return (await stepExp(env, e1)).andThen(e1p => {
            const bindings = [...e.bindings]
            bindings[0] = [x, e1p]
            return ok(L.elet(e.range, e.kind, bindings, e.body))
          })
        } else if (e.bindings.length === 1) {
          return ok(substitute(e1, x.value, e.body))
        } else if (e.kind === 'let') {
          return ok(L.elet(
            e.range,
            e.kind,
            e.bindings.slice(1),
            substitute(e1, x.value, e.body)
          ))
        } else if (e.kind === 'let*') {
          return ok(L.elet(
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
          if (L.isValue(guard)) {
            if (guard.tag === 'lit' && guard.value.tag === 'bool' && guard.value.value === true) {
              return ok(body)
            } else if (guard.tag === 'lit' && guard.value.tag === 'bool' && guard.value.value === false) {
              return ok(L.nlecond([...e.branches.slice(1)]))
            } else {
              return runtimeError(msg('error-type-expected-cond', guard.tag), e)
            }
          } else {
            return (await stepExp(env, guard)).andThen(guardp =>
              ok(L.nlecond([[guardp, body], ...e.branches.slice(1)])))
          }
        })
      }
    case 'and':
      if (e.args.length === 0) {
        return ok(L.nlebool(true))
      } else {
        const head = e.args[0]
        if (L.isValue(head)) {
          if (head.tag === 'lit' && head.value.tag === 'bool') {
            return head.value.value
              ? ok(L.nleand([...e.args.slice(1)]))
              : ok(L.nlebool(false))
          } else {
            return runtimeError(msg('error-type-expected', 'bool', head.tag), e)
          }
        } else {
          return (await stepExp(env, head)).andThen(headp =>
            ok(L.nleand([headp, ...e.args.slice(1)])))
        }
      }
    // N.B., or is identical to and expect dualized---factor redundancy?
    case 'or':
      if (e.args.length === 0) {
        return ok(L.nlebool(false))
      } else {
        const head = e.args[0]
        if (L.isValue(head)) {
          if (head.tag === 'lit' && head.value.tag === 'bool') {
            return !head.value.value
              ? ok(L.nleor([...e.args.slice(1)]))
              : ok(L.nlebool(true))
          } else {
            return runtimeError(msg('error-type-expected', 'bool', head.tag), e)
          }
        } else {
          return (await stepExp(env, head)).andThen(headp =>
            ok(L.nleor([headp, ...e.args.slice(1)])))
        }
      }
    case 'match':
      if (!L.isValue(e.scrutinee)) {
        return (await stepExp(env, e.scrutinee)).asyncAndThen(scrutinee => Promise.resolve(ok(L.ematch(e.range, scrutinee, e.branches, e.bracket))))
      } else {
        const branches = e.branches
        if (branches.length === 0) {
          return runtimeError(msg('error-match-no-branch-applies'), e)
        } else {
          for (let i = 0; i < branches.length; i++) {
            const bindings = tryMatch(e.scrutinee, branches[i][0])
            if (bindings) {
              return ok(substituteEnv(bindings, branches[i][1]))
            }
          }
          return runtimeError(msg('error-match-no-branch-applies'), e)
        }
      }
  }
}

async function evaluateExp (env: L.Env, e: L.Exp) : Promise<Result<L.Exp>> {
  while (!L.isValue(e)) {
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

function resultToStmt (result: Result<L.Stmt>): L.Stmt {
  switch (result.tag) {
    case 'error':
      return L.serror(result.details)
    case 'ok':
      return result.value
  }
}

async function stepStmt (env: L.Env, s: L.Stmt): Promise<[L.Env, L.Stmt]> {
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
      if (L.isValue(s.value)) {
        return [
          env.append(s.name.value, L.entry(s.value, 'binding', s.name.range)),
          L.sbinding(s.name.value)
        ]
      } else {
        return [
          env,
          resultToStmt((await stepExp(env, s.value)).andThen(e => ok(L.sdefine(s.name, e))))
        ]
      }
    case 'struct': {
      const name = s.id.value
      const predName = `${name}?`
      // primitive for type-testing predicate: id?
      const predPrim: L.Prim = (env, args, app) =>
        Promise.resolve(args.length !== 1
          ? runtimeError(msg('error-arity', predName, 1, args.length), app)
          : ok(L.nlebool(L.isStructKind(args[0], name))))
      // field-accessing primitives: id-field?
      const fieldPrims: [string, L.EnvEntry][] = s.fields.map(f => {
        const fieldName = `${name}-${f.value}`
        return [fieldName, entry(
          L.nleprim((env, args, app) =>
            Promise.resolve(args.length !== 1
              ? runtimeError(msg('error-arity', fieldName, 1, args.length), app)
              : !L.isStructKind(args[0], name)
                  ? runtimeError(msg('error-type-expected-fun', 1, fieldName, `struct ${name}`, args[0].tag))
                  // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
                  : ok((L.asStruct_(args[0]) as any)[f.value] as Exp))),
          `struct ${name}`,
          f.range
        )]
      })
      // constructor primitive: id
      const ctorPrim: L.Prim = (env, args, app) => {
        if (args.length !== s.fields.length) {
          return Promise.resolve(runtimeError(msg('error-arity', name, s.fields.length, args.length), app))
        } else {
          const obj: any = { }
          // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
          s.fields.forEach((f, i) => { obj[f.value] = args[i] })
          // eslint-disable-next-line @typescript-eslint/no-unsafe-argument
          return Promise.resolve(ok(L.nlestruct(name, obj)))
        }
      }
      return [
        env.concat(new L.Env([
          [name, entry(L.nleprim(ctorPrim), `struct ${name}`, s.id.range)],
          [predName, L.entry(L.nleprim(predPrim), `struct ${name}`, s.id.range)],
          ...fieldPrims
        ])),
        L.sbinding(`struct ${name}`)
      ]
    }
    case 'testcase': {
      const result: Result<L.Stmt> = await (await evaluateExp(env, s.desc)).asyncAndThen(async e1 =>
        (await evaluateExp(env, s.expected)).asyncAndThen(async expected =>
          (await evaluateExp(env, s.actual)).asyncAndThen(async actual =>
            (await evaluateExp(env, L.nlecall(s.comp, [expected, actual]))).andThen(e2 => {
              if (!L.isString(e1)) {
                return runtimeError(msg('error-type-expected', 'string', e1.tag), s.desc)
              } else if (!L.isBoolean(e2)) {
                return runtimeError(msg('error-type-expected', 'bool', e2.tag), s.comp)
              }
              const desc = L.asString_(e1)
              const passed = L.asBool_(e2)
              return ok(passed
                ? L.stestresult(desc, true)
                : L.stestresult(desc, false, undefined, expected, actual))
            }))))
      if (result.tag === 'error') {
        return [env, L.serror(result.details)]
      } else {
        return [env, result.value]
      }
    }
    // N.B., as a last step, substitute free variables away when they are values.
    case 'exp': {
      const result = L.isValue(s.value)
        ? resultToStmt(substituteIfFreeVar(env, s.value).andThen(vp => ok(L.svalue(vp))))
        : resultToStmt((await stepExp(env, s.value)).andThen(v =>
          L.isValue(v)
            ? substituteIfFreeVar(env, v).andThen(vp => ok(L.svalue(vp)))
            : ok(L.sexp(v)) as Result<L.Stmt>))
      return [env, result]
    }
    case 'import':
      if (internalLibs.has(s.source)) {
        return [
          new L.Env([...env.items(), ...internalLibs.get(s.source)!.items()]),
          L.simported(s.source)
        ]
      } else {
        return [
          env,
          L.serror([errorDetails(
            msg('phase-runtime'),
            msg('error-import-not-found', s.source, s.range)
          )])
        ]
      }
  }
}

const internalLibs: Map<string, L.Env> = new Map([
  ['image', imageLib],
  ['music', musicLib]
])

export {
  runtimeError, substitute,
  stepExp, stepStmt, evaluateExp,
  preludeEnv, internalLibs
}
