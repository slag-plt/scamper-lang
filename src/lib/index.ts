import { entry, Env } from '../env.js'
import { ImportDecl, nleprim, Program } from '../lang.js'
import { msg } from '../messages.js'
import { primMap } from '../prims.js'
import { Result, ok, errorDetails, errors, rethrow } from '../result.js'
import { imageLib } from './image.js'

const internalLibs: Map<string, Env> = new Map([
  ['image', imageLib]
])

function resolveImport (imp: ImportDecl, env: Env): Result<Env> {
  // 1. Try to resolve the import by looking at internal modules
  if (env.has(imp.source)) {
    return ok(new Env([...env.items(), ...internalLibs.get(imp.source)!.items()]))
  } else {
    return errors([errorDetails(
      msg('phase-runtime'),
      msg('error-import-not-found', imp.source),
      imp.range
    )])
  }
}

function resolveImportsInProgram (env: Env, program: Program): Result<Env> {
  program.imports.forEach(imp => {
    const result = resolveImport(imp, env)
    switch (result.tag) {
      case 'error':
        return rethrow(result)
      case 'ok':
        env = result.value
    }
  })
  return ok(env)
}

const preludeEnv: Env = new Env(Array.from(primMap.entries()).map(b => [b[0], entry(nleprim(b[1]), 'Prelude')]))

export { preludeEnv, resolveImportsInProgram }
