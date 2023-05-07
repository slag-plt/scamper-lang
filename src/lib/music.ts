/* eslint-disable camelcase */
/* eslint-disable no-use-before-define */
/* eslint-disable @typescript-eslint/no-unsafe-member-access */
/* eslint-disable @typescript-eslint/restrict-template-expressions */
/* eslint-disable @typescript-eslint/no-unsafe-call */
/* eslint-disable @typescript-eslint/no-unsafe-assignment */
import * as L from '../lang.js'
import { msg } from '../messages.js'
import { ICE, ok } from '../result.js'
import { runtimeError } from '../runtime.js'
import { evaluateExp } from '../evaluator.js'
import * as Docs from './docs.js'
import * as Pretty from '../pretty.js'
import * as Utils from './utils.js'

import { waf, instrMap, percMap } from './webaudiofont/webaudiofont.js'

export type PitchClass = string
export type Octave = number

export type Duration = {
  _scamperTag: 'struct',
  kind: 'dur',
  fields: [number, number]
}
export const dur = (num: number, den: number): Duration => ({ _scamperTag: 'struct', kind: 'dur', fields: [num, den] })

const isPitchClass = (s: string): boolean =>
  /^[A-Ga-g][#b]{0,2}$/.test(s)

const isOctave = (n: number): boolean =>
  n >= 0 && n <= 10

const isValidMidiNote = (n: number): boolean =>
  n >= 0 && n <= 127

export type Note = { renderAs: 'composition', tag: 'note', note: number, duration: Duration }
const note = (note: number, duration: Duration): Note => ({
  renderAs: 'composition', tag: 'note', note, duration
})

export type NoteFreq = { renderAs: 'composition', tag: 'note-freq', freq: number, duration: Duration }
const noteFreq = (freq: number, duration: Duration): NoteFreq => ({
  renderAs: 'composition', tag: 'note-freq', freq, duration
})

const repeatPrim: L.Prim = async (env, args, app) =>
  Utils.checkArgsResult('repeat', ['integer?', 'composition'], undefined, args, app).asyncAndThen(async _ => {
    const n = args[0] as number
    if (n < 0) {
      return runtimeError(msg('error-precondition-not-met', 'repeat', 1, 'non-negative integer', Pretty.expToString(0, L.nlevalue(args[0]))))
    } else {
      return evaluateExp(env,
        L.nleif(
          L.nlecall(L.nlevar('='), [L.nlevalue(n), L.nlenumber(0)]),
          L.nlevar('empty'),
          L.nlecall(L.nlevar('seq'), [
            L.nlevalue(args[1]),
            L.nlecall(L.nlevar('repeat'), [
              L.nlecall(L.nlevar('-'), [L.nlevalue(n), L.nlenumber(1)]),
              L.nlevalue(args[1])
            ])
          ])
        )
      )
    }
  })

type Empty = { renderAs: 'composition', tag: 'empty' }
const empty = (): Empty => ({ renderAs: 'composition', tag: 'empty' })

type Rest = { renderAs: 'composition', tag: 'rest', duration: Duration }
const rest = (duration: Duration): Rest => ({ renderAs: 'composition', tag: 'rest', duration })

type Trigger = { renderAs: 'composition', tag: 'trigger', fn: L.FunctionType }
const trigger = (fn: L.FunctionType): Trigger => {
  return ({ renderAs: 'composition', tag: 'trigger', fn })
}

type Par = { renderAs: 'composition', tag: 'par', notes: Composition[] }
const par = (notes: Composition[]): Par => ({ renderAs: 'composition', tag: 'par', notes })

type Seq = { renderAs: 'composition', tag: 'seq', notes: Composition[] }
const seq = (notes: Composition[]): Seq => ({ renderAs: 'composition', tag: 'seq', notes })

type Pickup = { renderAs: 'composition', tag: 'pickup', pickup: Composition, notes: Composition }
const pickup = (pickup: Composition, notes: Composition): Composition =>
  ({ renderAs: 'composition', tag: 'pickup', pickup, notes })

type ModKind = Percussion | PitchBend | Tempo | Dynamics | Instrument
type Percussion = { _scamperTag: 'struct', kind: 'mod', type: 'percussion', fields: []}
const percussion = (): Percussion => ({ _scamperTag: 'struct', kind: 'mod', type: 'percussion', fields: [] })

type PitchBend = { _scamperTag: 'struct', kind: 'mod', type: 'pitchBend', fields: [number] }
const pitchBend = (amount: number): PitchBend =>
  ({ _scamperTag: 'struct', kind: 'mod', type: 'pitchBend', fields: [amount] })
const pitchBendPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('bend', ['number?'], undefined, args, app).andThen(_ => {
    const amount = args[0] as number
    if (amount < -1 || amount > 1) {
      return runtimeError(msg('error-precondition-not-met', 'bend', 1, '-1 <= amount <= 1', Pretty.expToString(0, L.nlevalue(amount)), app))
    } else {
      return ok(pitchBend(amount))
    }
  }))

type Tempo = { _scamperTag: 'struct', kind: 'mod', type: 'tempo', fields: [Duration, number] }
const tempo = (beat: Duration, bpm: number): Tempo => ({ _scamperTag: 'struct', kind: 'mod', type: 'tempo', fields: [beat, bpm] })
const tempoPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('tempo', ['dur', 'number?'], undefined, args, app).andThen(_ => {
    const beat = args[0] as Duration
    const value = args[1] as number
    return value < 0
      ? runtimeError(msg('error-precondition-not-met', 'tempo', 1, 'tempo >= 0', Pretty.expToString(0, L.nlevalue(args[1]))), app)
      : ok(tempo(beat, value))
  }))

type Dynamics = { _scamperTag: 'struct', kind: 'mod', type: 'dynamics', fields: [number] }
const dynamics = (amount: number): Dynamics => ({ _scamperTag: 'struct', kind: 'mod', type: 'dynamics', fields: [amount] })
const dynamicsPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('dynamics', ['number?'], undefined, args, app).andThen(_ => {
    const amount = args[0] as number
    if (amount < 0 || amount > 127) {
      return runtimeError(msg('error-precondition-not-met', 'dynamics', 1, '0 <= amount <= 127', Pretty.expToString(0, L.nlevalue(args[0]))), app)
    } else {
      return ok(dynamics(amount))
    }
  }))

type Instrument = { _scamperTag: 'struct', kind: 'mod', type: 'instrument', fields: [number] }
const instrument = (program: number): Instrument =>
  ({ _scamperTag: 'struct', kind: 'mod', type: 'instrument', fields: [program] })
const instrumentPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('instrment', ['number?'], undefined, args, app).andThen(_ => {
    const amount = args[0] as number
    if (amount < 0 || amount > 127) {
      return runtimeError(msg('error-precondition-not-met', 'instrment', 1, '0 <= amount <= 127', Pretty.expToString(0, L.nlevalue(args[0]))), app)
    } else {
      return ok(instrument(amount))
    }
  }))

export type Mod = { renderAs: 'composition', tag: 'mod', note: Composition, mod: ModKind }
export const mod = (mod: ModKind, note: Composition): Mod => ({ renderAs: 'composition', tag: 'mod', note, mod })
export const modPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('mod', ['mod', 'composition'], undefined, args, app)
    .andThen(_ => ok(mod(args[0] as ModKind, args[1] as Composition))))

export type Composition = Empty | Note | NoteFreq | Rest | Trigger | Par | Seq | Pickup | Mod

const pitchQPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('pitch?', ['any'], undefined, args, app).andThen(_ =>
    ok(L.valueIsString(args[0]) && isPitchClass(args[0] as string))))

const octavePrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('octave?', ['any'], undefined, args, app).andThen(_ =>
    ok(L.valueIsInteger(args[0]) && isOctave(args[0] as number))))

const durQPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('dur?', ['any'], undefined, args, app).andThen(_ =>
    ok(L.valueIsStructKind(args[0], 'dur'))))

const durPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('dur', ['number?', 'number?'], undefined, args, app).andThen(_ =>
    ok({ _scamperTag: 'struct', kind: 'dur', fields: [args[0], args[1]] })))

const notePrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('note', ['integer?', 'dur'], undefined, args, app).andThen(_ => {
    const midiNote = args[0] as number
    const dur = args[1] as Duration
    if (!isValidMidiNote(midiNote)) {
      return runtimeError(msg('error-precondition-not-met', 'note', 1,
        '0 <= amount <= 128', Pretty.expToString(0, L.nlevalue(args[0])), app))
    } else {
      return ok(note(midiNote, dur))
    }
  }))

const noteFreqPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('note', ['number?', 'dur'], undefined, args, app).andThen(_ => {
    const freq = args[0] as number
    const dur = args[1] as Duration
    if (freq < 0 && freq > 4000) {
      return runtimeError(msg('error-precondition-not-met', 'note-freq', 1,
        '0 <= frequency <= 4000', Pretty.expToString(0, L.nlevalue(args[0])), app))
    } else {
      return ok(noteFreq(freq, dur))
    }
  }))

const restPrim: L.Prim = (_env, args, app) => {
  const argErr = Utils.checkArgs('rest', ['dur'], undefined, args, app)
  if (argErr) { return Promise.resolve(argErr) }
  const dur = args[0] as Duration
  return Promise.resolve(ok(rest(dur)))
}

const triggerPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('trigger', ['procedure?'], undefined, args, app).andThen(_ => {
    const proc = args[0] as L.FunctionType
    return ok(trigger(proc))
  }))

const parPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('par', [], 'composition', args, app).andThen(_ =>
    ok(par(args as Composition[]))))

const seqPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('seq', [], 'composition', args, app).andThen(_ =>
    ok(seq(args as Composition[]))))

const pickupPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('pickup', ['composition', 'composition'], undefined, args, app).andThen(_ =>
    ok(pickup(args[0] as Composition, args[1] as Composition))))

const numeratorPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('numerator', ['dur'], undefined, args, app).andThen(_ =>
    ok((args[0] as Duration).fields[0])))

const denominatorPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('denominator', ['dur'], undefined, args, app).andThen(_ =>
    ok((args[0] as Duration).fields[1])))

const playCompositionPrim: L.Prim = (env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('play-composition', ['composition'], undefined, args, app).andThen(_ => {
    playComposition(env, args[0] as Composition)
    return ok(undefined)
  }))

const musicEntry = (prim: L.Prim, docs?: L.Doc) => L.entry(L.vprim(prim), 'music', undefined, docs)

export const musicLib: L.Env = new L.Env([
  ['pitch?', musicEntry(pitchQPrim, Docs.pitch)],
  ['octave?', musicEntry(octavePrim, Docs.octave)],
  ['dur?', musicEntry(durQPrim, Docs.durQ)],
  ['dur', musicEntry(durPrim, Docs.dur)],
  ['numerator', musicEntry(numeratorPrim, Docs.numerator)],
  ['denominator', musicEntry(denominatorPrim, Docs.denominator)],
  ['empty', L.entry(empty(), 'music', undefined, Docs.empty)],
  ['note', musicEntry(notePrim, Docs.note)],
  ['note-freq', musicEntry(noteFreqPrim, Docs.noteFreq)],
  ['rest', musicEntry(restPrim, Docs.rest)],
  ['par', musicEntry(parPrim, Docs.par)],
  ['seq', musicEntry(seqPrim, Docs.seq)],
  ['pickup', musicEntry(pickupPrim, Docs.pickup)],
  ['mod', musicEntry(modPrim, Docs.mod)],
  ['percussion', L.entry(percussion(), 'music', undefined, Docs.percussion)],
  ['bend', musicEntry(pitchBendPrim, Docs.bend)],
  ['tempo', musicEntry(tempoPrim, Docs.tempo)],
  ['dynamics', musicEntry(dynamicsPrim, Docs.dynamics)],
  ['instrument', musicEntry(instrumentPrim, Docs.instrument)],
  ['trigger', musicEntry(triggerPrim, Docs.trigger)],
  ['repeat', musicEntry(repeatPrim, Docs.repeat)],
  ['wn', L.entry(dur(1, 1), 'music', undefined, Docs.wn)],
  ['hn', L.entry(dur(1, 2), 'music', undefined, Docs.hn)],
  ['qn', L.entry(dur(1, 4), 'music', undefined, Docs.qn)],
  ['en', L.entry(dur(1, 8), 'music', undefined, Docs.en)],
  ['sn', L.entry(dur(1, 16), 'music', undefined, Docs.sn)],
  ['tn', L.entry(dur(1, 32), 'music', undefined, Docs.tn)],
  ['play-composition', musicEntry(playCompositionPrim, Docs.playComposition)]
])

type MidiMsg = {
  tag: 'midi',
  time: number,
  duration: number,
  note: number,
  instrument: number,
  velocity: number
}

type TriggerMsg = {
  tag: 'trigger',
  time: number,
  callback: L.FunctionType
}

type Msg = MidiMsg | TriggerMsg

const midiMsg = (time: number, duration: number, note: number, instrument: number, velocity: number): Msg =>
  ({ tag: 'midi', time, duration, note, instrument, velocity })

const triggerMsg = (time: number, callback: L.FunctionType): Msg =>
  ({ tag: 'trigger', time, callback })

function ratioToDouble (ratio: Duration) {
  return ratio.fields[0] / ratio.fields[1]
}

function durationToTimeMs (beat: Duration, bpm: number, dur: Duration) {
  return ratioToDouble(dur) / (ratioToDouble(beat) * bpm) * 60 * 1000
}

function freqToNote (freq: number): number {
  return Math.log2(freq / 440) * 12 + 69
}

function compositionToMsgs (
  beat: Duration, bpm: number, velocity: number, startTime: number,
  instrument: number, composition: Composition): { endTime: number, msgs: Msg[] } {
  switch (composition.tag) {
    case 'empty':
      return { endTime: startTime, msgs: [] }
    case 'note': {
      const endTime = startTime + durationToTimeMs(beat, bpm, composition.duration)
      return {
        endTime,
        msgs: [
          midiMsg(
            startTime,
            durationToTimeMs(beat, bpm, composition.duration),
            composition.note,
            instrument,
            velocity / 127
          )
        ]
      }
    }
    case 'note-freq': {
      const endTime = startTime + durationToTimeMs(beat, bpm, composition.duration)
      return {
        endTime,
        msgs: [
          midiMsg(
            startTime,
            durationToTimeMs(beat, bpm, composition.duration),
            freqToNote(composition.freq),
            instrument,
            velocity / 127
          )
        ]
      }
    }
    case 'rest':
      return {
        endTime: startTime + durationToTimeMs(beat, bpm, composition.duration),
        msgs: []
      }
    case 'trigger': {
      return {
        endTime: startTime,
        msgs: [triggerMsg(startTime, composition.fn)]
      }
    }

    case 'par': {
      const msgs: Msg[] = []
      let endTime = 0
      composition.notes.forEach(note => {
        const result = compositionToMsgs(beat, bpm, velocity, startTime, instrument, note)
        msgs.push(...result.msgs)
        endTime = Math.max(result.endTime, endTime)
      })
      msgs.sort((c1, c2) => c1.time - c2.time)
      return { endTime, msgs }
    }

    case 'seq': {
      const msgs: Msg[] = []
      let time = startTime
      composition.notes.forEach(note => {
        const result = compositionToMsgs(beat, bpm, velocity, time, instrument, note)
        msgs.push(...result.msgs)
        time = result.endTime
      })
      msgs.sort((c1, c2) => c1.time - c2.time)
      return { endTime: time, msgs }
    }

    case 'pickup': {
      const pickup = compositionToMsgs(beat, bpm, velocity, startTime, instrument, composition.pickup)
      const pickupDuration = pickup.endTime - startTime
      let notes: { endTime: number, msgs: Msg[] } | undefined
      // If the pickup would start in negative time, then rebase the composition to start
      // with the pickup instead.
      if (startTime - pickupDuration < 0) {
        pickup.msgs.forEach(msg => {
          msg.time += pickupDuration
        })
        notes = compositionToMsgs(beat, bpm, velocity, pickupDuration, instrument, composition.notes)

      // Otherwise, rebase pickup to start before the composition.
      } else {
        pickup.msgs.forEach(msg => {
          msg.time -= pickupDuration
        })
        notes = compositionToMsgs(beat, bpm, velocity, startTime, instrument, composition.notes)
      }
      const msgs: Msg[] = []
      msgs.push(...pickup.msgs)
      msgs.push(...notes.msgs)
      return { endTime: notes.endTime, msgs }
    }

    case 'mod': {
      if (composition.mod.type === 'percussion') {
        return compositionToMsgs(beat, bpm, velocity, startTime, 128, composition.note)
      } else if (composition.mod.type === 'pitchBend') {
        const msgs: Msg[] = []
        const data = compositionToMsgs(beat, bpm, velocity, startTime, instrument, composition.note)
        // TODO: handle pitch bends
        // msgs.push(midiMsg(startTime, pitchBendF(0, composition.mod.fields[0])))
        // msgs.push(...data.msgs)
        // msgs.push(midiMsg(data.endTime, pitchBendF(0, 0)))
        return { msgs, endTime: data.endTime }
      } else if (composition.mod.type === 'tempo') {
        return compositionToMsgs(composition.mod.fields[0], composition.mod.fields[1], velocity, startTime, instrument, composition.note)
      } else if (composition.mod.type === 'dynamics') {
        return compositionToMsgs(beat, bpm, composition.mod.fields[0], startTime, instrument, composition.note)
      } else if (composition.mod.type === 'instrument') {
        return compositionToMsgs(beat, bpm, velocity, startTime, composition.mod.fields[0], composition.note)
      } else {
        throw new ICE('compositionToMsgs', `unknown mod tag: ${composition.mod}`)
      }
    }
  }
}

export function playComposition (env: L.Env, composition: Composition): number {
  const msgs = compositionToMsgs(dur(1, 4), 120, 64, 0, 0, composition).msgs
  const triggers = msgs.filter(msg => msg.tag === 'trigger')
  const startTime = waf().audioContext.currentTime
  console.log(startTime)

  // Enqueue notes
  for (const msg of msgs) {
    // const elapsed = audioContext.currentTime - startTime
    if (msg.tag === 'midi' && msg.instrument < 128) {
      waf().player.queueWaveTable(waf().audioContext, waf().audioContext.destination, instrMap.get(msg.instrument)!, startTime + msg.time / 1000, msg.note, msg.duration / 1000, msg.velocity)
    } else if (msg.tag === 'midi' && msg.instrument === 128) {
      waf().player.queueWaveTable(waf().audioContext, waf().audioContext.destination, percMap.get(msg.note)!, startTime + msg.time / 1000, msg.note, msg.duration / 1000, msg.velocity)
    }
  }

  // Set up a timer to discharge triggers
  let i = 0
  // eslint-disable-next-line @typescript-eslint/no-misused-promises
  const id = window.setInterval(async () => {
    // N.B., in milliseconds
    const now = waf().audioContext.currentTime
    while (i < triggers.length) {
      const trigger = triggers[i]
      if (trigger.time / 1000 + startTime <= now) {
        await evaluateExp(env, L.nlecall(L.nlevalue((trigger as TriggerMsg).callback), []))
        i += 1
        continue
      } else {
        return Promise.resolve(undefined)
      }
    }
    clearInterval(id)
    return Promise.resolve(undefined)
  })
  return id
}
