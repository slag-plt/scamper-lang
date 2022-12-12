/* eslint-disable @typescript-eslint/no-unsafe-member-access */
/* eslint-disable @typescript-eslint/restrict-template-expressions */
/* eslint-disable @typescript-eslint/no-unsafe-call */
/* eslint-disable @typescript-eslint/no-unsafe-assignment */
import * as Music from '../lib/music.js'
import * as Scamper from '../index.js'

export function emitCompositionWidget (node: Element) {
  const id = parseInt(node.id)
  const composition: Music.Composition = Scamper.store.get(id)! as Music.Composition
  node.textContent = '' // N.B., clear the contents of the node for the buttons
  const playButton = document.createElement('button')
  playButton.textContent = '▶'
  const stopButton = document.createElement('button')
  stopButton.textContent = '■'
  let timer: number | undefined
  playButton.onclick = function (_e) {
    timer = Music.playComposition(Scamper.preludeEnv, composition)
  }
  stopButton.onclick = function (_e) {
    if (timer !== undefined) {
      clearInterval(timer)
    }
  }
  node.appendChild(playButton)
  node.appendChild(stopButton)
}
