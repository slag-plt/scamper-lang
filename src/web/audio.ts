import { Store } from '../index.js'
import * as Audio from '../lib/audio.js'

function drawOscilloscope (data: Uint8Array, canvas: HTMLCanvasElement, analyser: AnalyserNode) {
  requestAnimationFrame(() => drawOscilloscope(data, canvas, analyser))

  const bufferLength = analyser.frequencyBinCount
  analyser.getByteTimeDomainData(data)
  const ctx = canvas.getContext('2d')!
  ctx.fillStyle = 'rgb(200, 200, 200)'
  ctx.fillRect(0, 0, canvas.width, canvas.height)

  ctx.lineWidth = 2
  ctx.strokeStyle = 'rgb(0, 0, 0)'
  ctx.beginPath()
  const sliceWidth = (canvas.width * 1.0) / bufferLength
  let x = 0

  for (let i = 0; i < bufferLength; i++) {
    const v = data[i] / 128.0
    const y = (v * canvas.height) / 2

    if (i === 0) {
      ctx.moveTo(x, y)
    } else {
      ctx.lineTo(x, y)
    }

    x += sliceWidth
  }

  ctx.lineTo(canvas.width, canvas.height / 2)
  ctx.stroke()
}

export function emitAudioWidget (store: Store, ctx: AudioContext, node: Element) {
  const pipeline = JSON.parse(node.textContent!) as Audio.AudioPipeline
  node.textContent = '' // N.B., clear the contents of the node for the buttons
  const playButton = document.createElement('button')
  playButton.textContent = '▶'
  const stopButton = document.createElement('button')
  stopButton.textContent = '■'
  const visualizer = document.createElement('canvas')

  const analyser = ctx.createAnalyser()
  analyser.fftSize = 2048
  const bufferLength = analyser.frequencyBinCount
  const dataArray = new Uint8Array(bufferLength)
  analyser.getByteTimeDomainData(dataArray)

  switch (pipeline.tag) {
    case 'sample': {
      if (pipeline.storeTag === undefined) {
        throw new Error('sample node has no store tag')
      }
      const data = store.get(pipeline.storeTag) as Float32Array
      // N.B., for now, make the audio sample stereo (2 channels)
      const buffer = ctx.createBuffer(2, data.length, ctx.sampleRate)
      buffer.copyToChannel(data, 0)
      buffer.copyToChannel(data, 1)
      let source: AudioBufferSourceNode | undefined
      playButton.onclick = () => {
        source = ctx.createBufferSource()
        source.buffer = buffer
        source.connect(ctx.destination)
        source.connect(analyser)
        source.start()
        drawOscilloscope(dataArray, visualizer, analyser)
      }
      stopButton.onclick = () => {
        if (source !== undefined) {
          source.stop()
        }
      }
    }
  }

  node.appendChild(playButton)
  node.appendChild(stopButton)
  node.appendChild(visualizer)
}
