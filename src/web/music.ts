/* eslint-disable @typescript-eslint/no-unsafe-member-access */
/* eslint-disable @typescript-eslint/restrict-template-expressions */
/* eslint-disable @typescript-eslint/no-unsafe-call */
/* eslint-disable @typescript-eslint/no-unsafe-assignment */
import { Composition, playComposition } from '../lib/music.js'
import { waf } from '../lib/webaudiofont/webaudiofont.js'
import * as Scamper from '../index.js'

export function renderer (obj: Object): HTMLElement {
  const composition: Composition = obj as Composition
  const ret = document.createElement('span')
  const playButton = document.createElement('button')
  playButton.textContent = '▶'
  const stopButton = document.createElement('button')
  stopButton.textContent = '■'
  let timer: number | undefined
  playButton.onclick = function (_e) {
    if (waf().audioContext.state === 'suspended') {
      waf().audioContext.resume().catch(console.error)
    }
    timer = playComposition(Scamper.preludeEnv, composition)
  }
  stopButton.onclick = function (_e) {
    if (timer !== undefined) {
      clearInterval(timer)
      waf().player.cancelQueue(waf().audioContext)
    }
  }
  ret.appendChild(playButton)
  ret.appendChild(stopButton)
  return ret
}
