import * as L from '../lang.js'
import { imageLib } from './image.js'
import { musicLib } from './music.js'
import { audioLib } from './audio.js'

export { preludeEnv } from './prelude.js'

export const internalLibs: Map<string, L.Env> = new Map([
  ['image', imageLib],
  ['music', musicLib],
  ['audio', audioLib]
])
