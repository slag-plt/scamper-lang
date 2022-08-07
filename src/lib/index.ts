/*
import { Env } from '../env.js'
import { Result, ok } from '../result.js'
import { imageLib } from './image.js'

function resolveImport (source: string, env: Env): Result<Env> {
  // 1. Try to resolve the import by looking at internal modules
  if (env.has(source)) {
    return ok(new Map([...env.entries(), ...internalLibs.get(source)!.entries()]))
  }
}

const internalLibs: Env = new Map([
  ['image', imageLib]
])
*/
