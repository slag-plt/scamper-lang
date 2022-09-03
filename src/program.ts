import * as L from './lang.js'
import * as Parser from './parser.js'
import * as Pretty from './pretty.js'
import * as R from './result.js'
import * as Runtime from './runtime.js'
import * as Scope from './scope.js'

export class ProgramState {
  env: L.Env
  prog: L.Program

  constructor (prog: L.Program, env?: L.Env) {
    this.env = env || Runtime.preludeEnv
    this.prog = prog
  }

  isFullyEvaluated (): boolean {
    return this.prog.statements.every(L.isStmtDone)
  }

  step (): ProgramState {
    for (let i = 0; i < this.prog.statements.length; i++) {
      const s = this.prog.statements[i]
      if (!L.isStmtDone(s)) {
        // N.B., make sure to not mutate things, but instead, create a new
        // ProgramState with the updates.
        const result = Runtime.stepStmt(this.env, s)
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

  stepExp (e: L.Exp): R.Result<L.Exp> {
    return Runtime.stepExp(this.env, e)
  }

  evaluateExp (e: L.Exp): R.Result<L.Exp> {
    return Runtime.evaluateExp(this.env, e)
  }

  toString (): string {
    return Pretty.progToString(0, this.prog, true)
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
    const i = L.indexOfCurrentStmt(this.getCurrentState().prog)
    while (L.indexOfCurrentStmt(this.getCurrentState().prog) === i) {
      this.stepForward()
    }
  }

  revertPrevStmt (): void {
    const i = L.indexOfCurrentStmt(this.getCurrentState().prog)
    while (L.indexOfCurrentStmt(this.getCurrentState().prog) === i && this.pos > 0) {
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
    const result = Parser.parseProgram(src).andThen(prog =>
      R.detailsToResult(Scope.scopeCheckProgram(
        prog,
        this.states[this.states.length - 1].env)).andThen(_ => {
          this.states.forEach(st => {
            st.prog.statements = st.prog.statements.concat(prog.statements)
          })
        return R.ok(null)
      }))
    switch (result.tag) {
      case 'ok':
        return
      case 'error':
        this.states.forEach(st => {
          st.prog.statements.push(L.serror(result.details))
        })
    }
  }
}
