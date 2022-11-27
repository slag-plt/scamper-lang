import * as L from '../lang.js'
import { preludeEnv } from './prelude.js'
import { imageLib } from './image.js'
import { jsLib } from './js.js'
import { musicLib } from './music.js'
import { audioLib } from './audio.js'
import { canvasLib } from './canvas.js'

export { preludeEnv } from './prelude.js'

export const internalLibs: Map<string, L.Env> = new Map([
  ['prelude', preludeEnv],
  ['audio', audioLib],
  ['canvas', canvasLib],
  ['image', imageLib],
  ['js', jsLib],
  ['music', musicLib]
])
