import { evaluateExp, preludeEnv, stepExp, stepStmt } from './runtime.js'
import { Env, Exp, Stmt, isStmtDone, Program, indexOfCurrentStmt, progToString, serror } from './lang.js'
import { parseExp, parseProgram } from './parser.js'
import { detailsToResult, ok, Result } from './result.js'
import { scopeCheckExp, scopeCheckProgram } from './scope.js'

export * from './result.js'
export { expToString, stmtToString, progToString } from './lang.js'
export * from './web.js'

export function compileProgram (src: string): Result<Program> {
  return parseProgram(src).andThen(prog =>
    detailsToResult(scopeCheckProgram(prog)).andThen(_ =>
      ok(prog)))
}

export function compileExpr (env: Env, src: string): Result<Exp> {
  return parseExp(src).andThen(e =>
    detailsToResult(scopeCheckExp(e, env)).andThen(_ =>
      ok(e)))
}

export class ProgramState {
  env: Env
  prog: Program

  constructor (prog: Program, env?: Env) {
    this.env = env || preludeEnv
    this.prog = prog
  }

  isFullyEvaluated (): boolean {
    return this.prog.statements.every(isStmtDone)
  }

  step (): ProgramState {
    for (let i = 0; i < this.prog.statements.length; i++) {
      const s = this.prog.statements[i]
      if (!isStmtDone(s)) {
        // N.B., make sure to not mutate things, but instead, create a new
        // ProgramState with the updates.
        const result = stepStmt(this.env, s)
        const prog = {
          statements: [...this.prog.statements]
        }
        prog.statements[i] = result[1]
        return new ProgramState(prog, result[0])
      }
    }
    return this
  }

  evaluate (): ProgramState {
    let st: ProgramState = this
    while (!st.isFullyEvaluated()) {
      st = st.step()
    }
    return st
  }

  stepExp (e: Exp): Result<Exp> {
    return stepExp(this.env, e)
  }

  evaluateExp (e: Exp): Result<Exp> {
    return evaluateExp(this.env, e)
  }

  toString (): string {
    return progToString(this.prog, true)
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

  stepForward (): void {
    const lastI = this.states.length - 1
    if (this.pos === lastI && !this.states[lastI].isFullyEvaluated()) {
      this.states.push(this.states[lastI].step())
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

  evalNextStmt (): void {
    if (this.getCurrentState().isFullyEvaluated()) { return }
    const i = indexOfCurrentStmt(this.getCurrentState().prog)
    while (indexOfCurrentStmt(this.getCurrentState().prog) === i) {
      this.stepForward()
    }
  }

  revertPrevStmt (): void {
    const i = indexOfCurrentStmt(this.getCurrentState().prog)
    while (indexOfCurrentStmt(this.getCurrentState().prog) === i && this.pos > 0) {
      this.stepBackward()
    }
  }

  evaluateProg (): void {
    while (!this.states[this.pos].isFullyEvaluated()) {
      this.stepForward()
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

  addStmt (src: string): void {
    // N.B., evaluate the program completely so we compute the final set of bindings
    this.evaluateProg()
    const result = parseProgram(src).andThen(prog =>
      detailsToResult(scopeCheckProgram(prog, this.states[this.states.length - 1].env)).andThen(_ => {
        this.states.forEach(st => {
          st.prog.statements = st.prog.statements.concat(prog.statements)
        })
        return ok(null)
      }))
    switch (result.tag) {
      case 'ok':
        return
      case 'error':
        this.states.forEach(st => {
          st.prog.statements.push(serror(result.details))
        })
    }
  }
}
