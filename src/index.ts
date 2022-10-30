import { Env, Value, Program } from './lang.js'
import { parseExp, parseProgram } from './parser.js'
import { detailsToResult, ok, Result } from './result.js'
import { scopeCheckExp, scopeCheckProgram } from './scope.js'

export { Env, EnvEntry } from './lang.js'

export { ProgramState, ProgramTrace } from './stepper.js'

export * from './result.js'
export * from './pretty.js'
export * as Formatter from './formatter.js'
export * as parser from './parser.js'
export * as sexp from './sexp.js'
export * as scope from './scope.js'
export * as Evaluator from './evaluator.js'
export * as Image from './lib/image.js'
export * as Music from './lib/music.js'
export * as Pretty from './pretty.js'
export * as Vfs from './vfs.js'

export function compileProgram (src: string): Result<Program> {
  return parseProgram(src).andThen(prog =>
    detailsToResult(scopeCheckProgram(prog)).andThen(_ =>
      ok(prog)))
}

export function compileExpr (env: Env, src: string): Result<Value> {
  return parseExp(src).andThen(e =>
    detailsToResult(scopeCheckExp(e, env)).andThen(_ =>
      ok(e)))
}
