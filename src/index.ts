import { Env, Value, Program } from './lang.js'
import { parseExp, parseProgram } from './parser.js'
import { detailsToResult, ok, Result } from './result.js'
import { scopeCheckExp, scopeCheckProgram } from './scope.js'

export * from './evaluator.js'
export * from './loc.js'
export * from './pretty.js'
export * from './result.js'
export * from './stepper.js'

export * as Lang from './lang.js'
export * as Formatter from './formatter.js'
export * as parser from './parser.js'
export * as Pretty from './pretty.js'
export * as Result from './result.js'
export * as sexp from './sexp.js'
export * as scope from './scope.js'
export * as Image from './lib/image.js'
export * as Music from './lib/music.js'
export { Store, store } from './runtime.js'
export { preludeEnv, internalLibs } from './lib/exports.js'

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
