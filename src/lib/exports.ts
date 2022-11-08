import * as L from '../lang.js'
import { imageLib } from './image.js'
import { jsLib } from './js.js'
import { musicLib } from './music.js'
import { audioLib } from './audio.js'

export { preludeEnv } from './prelude.js'

export const internalLibs: Map<string, L.Env> = new Map([
  ['audio', audioLib],
  ['image', imageLib],
  ['js', jsLib],
  ['music', musicLib]
])
