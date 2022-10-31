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
  Promise.resolve(Utils.checkArgsResult('sample-node', ['list?'], undefined, args, app).andThen(_ => {
    const data = L.valueListToArray_(args[0])
    for (let i = 0; i < data.length; i++) {
      if (typeof data[i] !== 'number' || data[i] as number < -1.0 || data[i] as number > 1.0) {
        return runtimeError(msg('error-precondition-not-met',
          'simple-note', 1, 'list of numbers between -1.0 and 1.0', Pretty.valueToString(0, data[i])))
      }
    }
    return ok(sampleNode(new Float32Array(data as number[])))
  }))

const audioEnvEntry = (prim: L.Prim, doc: L.Doc): L.EnvEntry => ({ value: L.vprim(prim), source: 'audio', range: noRange(), doc })

export const audioLib: L.Env = new L.Env([
  ['sample-node', audioEnvEntry(sampleNodePrim, Docs.sampleNode)]
])
