import { Env, Exp, Program } from './lang.js'
import { parseExp, parseProgram } from './parser.js'
import { detailsToResult, ok, Result } from './result.js'
import { scopeCheckExp, scopeCheckProgram } from './scope.js'

export { Env, EnvEntry } from './lang.js'
export * from './result.js'
export { preludeEnv, internalLibs } from './runtime.js'
export * from './pretty.js'
export * as Formatter from './formatter.js'
export * as parser from './parser.js'
export * as sexp from './sexp.js'
export * as scope from './scope.js'
export * as Image from './lib/image.js'
export * as Music from './lib/music.js'
export * as Vfs from './vfs.js'

export { ProgramState, ProgramTrace } from './program.js'

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
