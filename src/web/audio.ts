/* eslint-disable @typescript-eslint/no-unsafe-call */
/* eslint-disable @typescript-eslint/no-unsafe-member-access */
/* eslint-disable @typescript-eslint/no-unsafe-assignment */
import * as Audio from '../lib/audio.js'

const ctx = new AudioContext({ sampleRate: 16000 }) // TODO: need to parameterize this!

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

export function audioRenderer (obj: object): HTMLElement {
  const pipeline = obj as Audio.AudioPipeline
  const ret = document.createElement('span')
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
      const data = pipeline.data
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

  ret.appendChild(playButton)
  ret.appendChild(stopButton)
  ret.appendChild(visualizer)
  return ret
}

export function audioPipelineRenderer (obj: object): HTMLElement {
  const blob: { renderAs: 'audio-pipeline', ctx: AudioContext, pipeline: AudioScheduledSourceNode, onOffNode: GainNode } = obj as any
  const pipeline: AudioScheduledSourceNode = blob.pipeline
  const onOffNode: GainNode = blob.onOffNode

  const ret = document.createElement('span')
  const playButton = document.createElement('button')
  playButton.textContent = '▶'
  const stopButton = document.createElement('button')
  stopButton.textContent = '■'
  const startable = typeof (pipeline as any).start !== 'undefined'
  const sourceIsFile = typeof (pipeline as any).mediaElement !== 'undefined' && typeof (pipeline as any).mediaElement.play !== 'undefined'
  let started = false
  onOffNode.gain.value = 0
  playButton.onclick = _ => {
    onOffNode.gain.value = 1
    if (startable && !started) {
      pipeline.start()
      started = true
    } else if (sourceIsFile) {
      (pipeline as any).mediaElement.play()
      started = true
    }
  }
  stopButton.onclick = _ => {
    onOffNode.gain.value = 0
    if (sourceIsFile) {
      (pipeline as any).mediaElement.load()
      started = false
    }
  }
  ret.appendChild(playButton)
  ret.appendChild(stopButton)
  return ret
}
