import * as L from './lang.js'
import * as Libs from './lib/exports.js'
import * as Parser from './parser.js'
import * as Pretty from './pretty.js'
import * as Scope from './scope.js'

import { Result, detailsToResult, join, ok, rethrow, errorDetails, ICE } from './result.js'
import { msg } from './messages.js'
import { runtimeError, tryMatch } from './runtime.js'

// #region Support Functions

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
    case 'begin':
      return L.ebegin(e.range, e.exps.map(e => substitute(v, x, e)), e.bracket)
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

function substituteAll (es: L.Value[], xs: L.Name[], e: L.Exp) {
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
      if (env.has(e.value)) {
        return ok(L.nlevalue(env.get(e.value)!.value))
      } else {
        return runtimeError(msg('error-var-undef', e.value), e)
      }
    default:
      return ok(e)
  }
}

// #endregion

// #region Stepping Functions

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
      return ok(L.nlevalue(L.litToValue(e.value)))
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
        // Then, substitute away free variables in the head before resolving the call.
        return substituteIfFreeVar(env, e.head).asyncAndThen(head =>
          join(e.args.map(x => substituteIfFreeVar(env, x))).asyncAndThen(async (args: L.Exp[]): Promise<Result<L.Exp>> => {
            if (head.tag === 'value') {
              if (L.valueIsLambda(head.value)) {
                const lam = head.value as L.LambdaType
                if (args.length === lam.args.length) {
                  return ok(substituteAll(args.map(L.unpackIfValue), lam.args, lam.body))
                } else {
                  return runtimeError(msg('error-arity', 'lambda', lam.args.length, args.length), e)
                }
              } else if (L.valueIsPrim(head.value)) {
                const prim: L.Prim = (head.value as L.PrimType).fn
                return (await prim(env, args.map(L.unpackIfValue), e)).andThen(v => ok(L.nlevalue(v)))
              } else {
                return runtimeError(msg('error-type-expected-call', e.head.tag), e)
              }
            } else {
              throw new ICE('stepExp', 'call head is not a value')
            }
          }))
      }
    case 'lam':
      // N.B., load the closure with the current global environment
      return ok(L.nlevalue(L.vlambda(e.args, e.body, env)))
    case 'if':
      if (!L.isValue(e.e1)) {
        return (await stepExp(env, e.e1)).andThen(e1p => ok(L.eif(e.range, e1p, e.e2, e.e3)))
      } else {
        return substituteIfFreeVar(env, e.e1).andThen(e1 => {
          const v1 = L.unpackIfValue(e1)
          if (L.valueIsBoolean(v1)) {
            return v1 as boolean ? ok(e.e2) : ok(e.e3)
          } else {
            return runtimeError(msg('error-type-expected-cond', e1), e)
          }
        })
      }
    case 'nil':
      return ok(L.nlevalue(null))
    case 'pair':
      if (!L.isValue(e.e1)) {
        return (await stepExp(env, e.e1)).andThen(e1p => ok(L.epair(e.range, e1p, e.e2)))
      } else if (!L.isValue(e.e2)) {
        return (await stepExp(env, e.e2)).andThen(e2p => ok(L.epair(e.range, e.e1, e2p)))
      } else {
        return ok(L.nlevalue(L.vpair(L.unpackIfValue(e.e1), L.unpackIfValue(e.e2))))
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
          return ok(substitute(L.unpackIfValue(e1), x.value, e.body))
        } else if (e.kind === 'let') {
          return ok(L.elet(
            e.range,
            e.kind,
            e.bindings.slice(1),
            substitute(L.unpackIfValue(e1), x.value, e.body)
          ))
        } else if (e.kind === 'let*') {
          return ok(L.elet(
            e.range,
            e.kind,
            substituteInBindings(L.unpackIfValue(e1), x.value, e.bindings.slice(1), e.kind),
            inBindings(x.value, e.bindings.slice(1)) ? e.body : substitute(L.unpackIfValue(e1), x.value, e.body)
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
            const v = L.unpackIfValue(guard)
            if (L.valueIsBoolean(v) && v as boolean) {
              return ok(body)
            } else if (L.valueIsBoolean(v)) {
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
        return ok(L.nlevalue(true))
      } else {
        const head = e.args[0]
        if (L.isValue(head)) {
          const v = L.unpackIfValue(head)
          if (L.valueIsBoolean(v)) {
            return v
              ? ok(L.nleand([...e.args.slice(1)]))
              : ok(L.nlevalue(false))
          } else {
            return runtimeError(msg('error-type-expected', 'bool', head), e)
          }
        } else {
          return (await stepExp(env, head)).andThen(headp =>
            ok(L.nleand([headp, ...e.args.slice(1)])))
        }
      }
    // N.B., or is identical to and expect dualized---factor redundancy?
    case 'or':
      if (e.args.length === 0) {
        return ok(L.nlevalue(false))
      } else {
        const head = e.args[0]
        if (L.isValue(head)) {
          const v = L.unpackIfValue(head)
          if (L.valueIsBoolean(v)) {
            return v
              ? ok(L.nlevalue(true))
              : ok(L.nleor([...e.args.slice(1)]))
          } else {
            return runtimeError(msg('error-type-expected', 'bool', head), e)
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
            // TODO: probably bug: need to free-var substitute for e, otherwise pattern matching on
            // free variables will fail.
            const bindings = tryMatch(L.unpackIfValue(e.scrutinee), branches[i][0])
            if (bindings) {
              return ok(substituteEnv(bindings, branches[i][1]))
            }
          }
          return runtimeError(msg('error-match-no-branch-applies'), e)
        }
      }
    case 'begin':
      throw new ICE('stepExp', 'begin is not supported in the stepper')
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
        const result: Result<[L.Env, L.Stmt]> = substituteIfFreeVar(env, s.value).andThen(v => ok([
          env.append(s.name.value, L.entry(L.unpackValue(v), 'binding', s.name.range)),
          L.sbinding(s.name.value)
        ]))
        if (result.tag === 'ok') {
          return result.value
        } else {
          return [env, L.serror(result.details)]
        }
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
          : ok(L.valueIsStructKind(args[0], name)))
      // field-accessing primitives: id-field?
      const fieldPrims: [string, L.EnvEntry][] = s.fields.map((f, i) => {
        const fieldName: string = `${name}-${f.value}`
        return [fieldName, L.entry(
          L.vprim((_env, args, app) =>
            Promise.resolve(args.length !== 1
              ? runtimeError(msg('error-arity', fieldName, 1, args.length), app)
              : !L.valueIsStructKind(args[0], name)
                  ? runtimeError(msg('error-type-expected-fun', 1, fieldName, `struct ${name}`, args[0]))
                  // N.B., fields are encoded positionally. Field names exist purely at the source level.
                  : ok((((args[0] as L.StructType).fields[i]))))),
          `struct ${name}`,
          f.range
        )]
      })
      // constructor primitive: id
      const ctorPrim: L.Prim = (env, args, app) => {
        if (args.length !== s.fields.length) {
          return Promise.resolve(runtimeError(msg('error-arity', name, s.fields.length, args.length), app))
        } else {
          return Promise.resolve(ok(L.vstruct(name, [...args])))
        }
      }
      return [
        env.concat(new L.Env([
          [name, L.entry(L.vprim(ctorPrim), `struct ${name}`, s.id.range)],
          [predName, L.entry(L.vprim(predPrim), `struct ${name}`, s.id.range)],
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
              const v1 = L.unpackValue(e1)
              const v2 = L.unpackValue(e2)
              if (!L.valueIsString(v1)) {
                return runtimeError(msg('error-type-expected', 'string', e1), s.desc)
              } else if (!L.valueIsBoolean(v2)) {
                return runtimeError(msg('error-type-expected', 'bool', e2), s.comp)
              }
              const desc = v1 as string
              const passed = v2 as boolean
              return ok(passed
                ? L.stestresult(desc, true)
                // TODO: should probably work with values rather than exps here
                : L.stestresult(desc, false, undefined, expected, actual))
            }))))
      if (result.tag === 'error') {
        return [env, L.serror(result.details)]
      } else {
        return [env, result.value]
      }
    }
    // N.B., as a last step, substitute free variables away when they are values.
    // TODO: probably should thread htmlOutput through here, but will be refactoring soon, anyways...
    case 'exp': {
      const result = L.isValue(s.value)
        ? resultToStmt(substituteIfFreeVar(env, s.value).andThen(vp => ok(L.svalue(L.unpackIfValue(vp)))))
        : resultToStmt((await stepExp(env, s.value)).andThen(v =>
          L.isValue(v)
            ? substituteIfFreeVar(env, v).andThen(vp => ok(L.svalue(L.unpackIfValue(vp))))
            : ok(L.sexp(v)) as Result<L.Stmt>))
      return [env, result]
    }
    case 'import':
      if (Libs.internalLibs.has(s.source)) {
        return [
          new L.Env([...env.items(), ...Libs.internalLibs.get(s.source)!.items()]),
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

// #endregion

// #region Programs and Traces

export class ProgramState {
  env: L.Env
  prog: L.Program

  constructor (prog: L.Program, env?: L.Env) {
    this.env = env || Libs.preludeEnv
    this.prog = prog
  }

  isFullyEvaluated (): boolean {
    return this.prog.every(L.isStmtDone)
  }

  async step (): Promise<ProgramState> {
    for (let i = 0; i < this.prog.length; i++) {
      const s = this.prog[i]
      if (!L.isStmtDone(s)) {
        // N.B., make sure to not mutate things, but instead, create a new
        // ProgramState with the updates.
        // const result = await Runtime.stepStmt(this.env, s)
        const result = await stepStmt(this.env, s)
        const prog = [...this.prog]
        prog[i] = result[1]
        return new ProgramState(prog, result[0])
      }
    }
    return this
  }

  async evaluate (): Promise<ProgramState> {
    let st: ProgramState = this
    while (!st.isFullyEvaluated()) {
      st = await st.step()
    }
    return st
  }

  stepExp (e: L.Exp): Promise<Result<L.Value>> {
    return stepExp(this.env, e)
  }

  evaluateExp (e: L.Exp): Promise<Result<L.Value>> {
    return evaluateExp(this.env, e)
  }

  toString (outputBindings?: boolean): string {
    return Pretty.progToString(0, this.prog, outputBindings)
  }
}

export class ProgramTrace {
  states: ProgramState[]
  pos: number

  constructor (initial: ProgramState) {
    this.states = [initial]
    this.pos = 0
  }

  getCurrentState (): ProgramState {
    return this.states[this.pos]
  }

  async stepForward (): Promise<void> {
    const lastI = this.states.length - 1
    if (this.pos === lastI && !this.states[lastI].isFullyEvaluated()) {
      this.states.push(await this.states[lastI].step())
      this.pos += 1
    } else if (this.pos < lastI) {
      this.pos += 1
    }
    // N.B., if we're on the last state and it is fully evaluated, then we
    // do not advance forward.
  }

  stepBackward (): void {
    if (this.pos > 0) {
      this.pos--
    }
  }

  async evalNextStmt (): Promise<void> {
    if (this.getCurrentState().isFullyEvaluated()) { return }
    const i = L.indexOfCurrentStmt(this.getCurrentState().prog)
    while (L.indexOfCurrentStmt(this.getCurrentState().prog) === i) {
      await this.stepForward()
    }
  }

  revertPrevStmt (): void {
    const i = L.indexOfCurrentStmt(this.getCurrentState().prog)
    while (L.indexOfCurrentStmt(this.getCurrentState().prog) === i && this.pos > 0) {
      this.stepBackward()
    }
  }

  async evaluateProg (): Promise<void> {
    while (!this.states[this.pos].isFullyEvaluated()) {
      await this.stepForward()
    }
  }

  resetProg (): void {
    this.pos = 0
  }

  currentStep (): number {
    return this.pos + 1
  }

  currentState (): ProgramState {
    return this.states[this.pos]
  }

  async addStmt (src: string): Promise<void> {
    // N.B., evaluate the program completely so we compute the final set of bindings
    await this.evaluateProg()
    const result = Parser.parseProgram(src).andThen(prog =>
      detailsToResult(Scope.scopeCheckProgram(
        prog,
        this.states[this.states.length - 1].env)).andThen(_ => {
        this.states.forEach(st => {
          st.prog = st.prog.concat(prog)
        })
        return ok(null)
      }))
    switch (result.tag) {
      case 'ok':
        return
      case 'error':
        this.states.forEach(st => {
          st.prog.push(L.serror(result.details))
        })
    }
  }
}

// #endregion
