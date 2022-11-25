/* eslint-disable no-use-before-define */

import { msg } from '../messages.js'
import { runtimeError } from '../runtime.js'
import * as Docs from './docs.js'
import { noRange } from '../loc.js'
import * as L from '../lang.js'
import * as Pretty from '../pretty.js'
import * as Utils from './utils.js'
import { ok } from '../result.js'

export type AudioPipeline = SampleNode

export type SampleNode = { renderAs: 'audio', tag: 'sample', data: Float32Array, storeTag?: number }
export const sampleNode = (data: Float32Array): AudioPipeline => ({ renderAs: 'audio', tag: 'sample', data, storeTag: undefined })

const sampleNodePrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('sample-node', ['vector?'], undefined, args, app).andThen(_ => {
    const data = args[0] as L.Value[]
    for (let i = 0; i < data.length; i++) {
      if (typeof data[i] !== 'number' || data[i] as number < -1.0 || data[i] as number > 1.0) {
        return runtimeError(msg('error-precondition-not-met',
          'sample-node', 1, 'list of numbers between -1.0 and 1.0', Pretty.valueToString(0, data[i])))
      }
    }
    return ok(sampleNode(new Float32Array(data as number[])))
  }))

const audioContextDoc: L.Doc = new L.Doc(
  '(audio-context sampleRate) -> context?', [
    'sampleRate: integer?, positive'
  ],
  'Creates an audio context with the given sample rate.'
)

const audioContextPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('audio-context', ['integer?'], undefined, args, app).andThen(_ => {
    const AudioContext = window.AudioContext
    return ok(new AudioContext({ sampleRate: args[0] as number }))
  }))

const audioPipelineDoc: L.Doc = new L.Doc(
  '(audio-pipeline ctx n1 ... nk) -> pipeline?', [
    'ctx: context?',
    'n1 ... nk: audio-node?'
  ],
  'Creates an audio pipeline from the given audio nodes, connecting the nodes in sequence.'
)

const audioPipelinePrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('audio-pipeline', ['any', 'any'], 'any', args, app).andThen(_ => {
    const ctx = args[0] as AudioContext
    const pipeline = args[1] as AudioNode
    for (let i = 1; i < args.length - 1; i++) {
      const node = args[i] as AudioNode
      node.connect(args[i + 1] as AudioNode)
    }
    // HUH?
    const onOffNode = new GainNode(ctx);
    (args[args.length - 1] as AudioNode).connect(onOffNode)
    onOffNode.connect(ctx.destination)
    return ok({
      renderAs: 'audio-pipeline',
      ctx,
      pipeline,
      onOffNode
    })
  }))

const oscillatorNodeDoc: L.Doc = new L.Doc(
  '(oscillator-node ctx type freq) -> node?', [
    'ctx: context?',
    'type: string?',
    'freq: number?, positive'
  ],
  'Creates an oscillator node with the given type and frequency.'
)

const oscillatorNodePrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('oscillator-node', ['any', 'string?', 'integer?'], undefined, args, app).andThen(_ => {
    const ctx = args[0] as AudioContext
    const type = args[1] as OscillatorType
    const freq = args[2] as number
    const oscillator = ctx.createOscillator()
    oscillator.type = type
    oscillator.frequency.value = freq
    return ok(oscillator)
  }))

const microphoneNodeDoc: L.Doc = new L.Doc(
  '(microphone-node ctx) -> node?', [
    'ctx: context?'
  ],
  'Creates an audio source node connected to the user\'s microphone.'
)

const microphoneNodePrim: L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('microphone-node', ['any'], undefined, args, app).asyncAndThen(async _ => {
    const ctx = args[0] as AudioContext
    const mediaStream = await navigator.mediaDevices.getUserMedia({ audio: true, video: false })
    const source = new MediaStreamAudioSourceNode(ctx, { mediaStream })
    return ok(source)
  })

const delayNodeDoc: L.Doc = new L.Doc(
  '(delay-node ctx delay) -> node?', [
    'ctx: context?',
    'delay: number?, positive'
  ],
  'Creates a delay node with the given delay time.'
)

const delayNodePrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('delay-node', ['any', 'number?'], undefined, args, app).andThen(_ => {
    const ctx = args[0] as AudioContext
    const delayTime = args[1] as number
    const source = new DelayNode(ctx, { delayTime })
    return ok(source)
  }))

const audioEnvEntry = (prim: L.Prim, doc: L.Doc): L.EnvEntry => ({ value: L.vprim(prim), source: 'audio', range: noRange(), doc })

export const audioLib: L.Env = new L.Env([
  ['sample-node', audioEnvEntry(sampleNodePrim, Docs.sampleNode)],
  ['audio-context', audioEnvEntry(audioContextPrim, audioContextDoc)],
  ['audio-pipeline', audioEnvEntry(audioPipelinePrim, audioPipelineDoc)],
  ['oscillator-node', audioEnvEntry(oscillatorNodePrim, oscillatorNodeDoc)],
  ['microphone-node', audioEnvEntry(microphoneNodePrim, microphoneNodeDoc)],
  ['delay-node', audioEnvEntry(delayNodePrim, delayNodeDoc)]
])
