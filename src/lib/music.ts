/* eslint-disable no-use-before-define */
/* eslint-disable @typescript-eslint/no-unsafe-member-access */
/* eslint-disable @typescript-eslint/restrict-template-expressions */
/* eslint-disable @typescript-eslint/no-unsafe-call */
/* eslint-disable @typescript-eslint/no-unsafe-assignment */
import * as JZZ from './jzz/jzz-combined.cjs'
import * as L from '../lang.js'
import { msg } from '../messages.js'
import { ICE, ok } from '../result.js'
import { runtimeError } from '../runtime.js'
import { evaluateExp } from '../evaluator.js'
import * as Docs from './docs.js'
import * as Pretty from '../pretty.js'
import * as Utils from './utils.js'

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
  console.log(fn)
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

export type Composition = Empty | Note | Rest | Trigger | Par | Seq | Pickup | Mod

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

type MIDI = any // TODO: ugh, how do I specify this type!?
type Synth = any // TODO: ...this one, too!

type MidiMsg = {
  tag: 'midi',
  time: number,
  data: MIDI,
}

type TriggerMsg = {
  tag: 'trigger',
  time: number,
  callback: L.FunctionType
}

const testProgramMap = [
  56, // piano
  5, // electric piano
  19, // rock organ
  26, // jazz guitar
  31, // distorted gutiar
  33, // electric bass
  40, // violin
  56, // trumpet
  65 // alto sax
]

type Msg = MidiMsg | TriggerMsg

const midiMsg = (time: number, data: MIDI): Msg =>
  ({ tag: 'midi', time, data })

const triggerMsg = (time: number, callback: L.FunctionType): Msg =>
  ({ tag: 'trigger', time, callback })

function ratioToDouble (ratio: Duration) {
  return ratio.fields[0] / ratio.fields[1]
}

function durationToTimeMs (beat: Duration, bpm: number, dur: Duration) {
  return ratioToDouble(dur) / (ratioToDouble(beat) * bpm) * 60 * 1000
}

function compositionToMsgs (
  beat: Duration, bpm: number, velocity: number, startTime: number,
  program: number, composition: Composition): { endTime: number, msgs: Msg[] } {
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
            JZZ.MIDI.noteOn(program, composition.note, velocity)
          ),
          midiMsg(
            endTime,
            JZZ.MIDI.noteOff(program, composition.note, velocity)
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
        const result = compositionToMsgs(beat, bpm, velocity, startTime, program, note)
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
        const result = compositionToMsgs(beat, bpm, velocity, time, program, note)
        msgs.push(...result.msgs)
        time = result.endTime
      })
      msgs.sort((c1, c2) => c1.time - c2.time)
      return { endTime: time, msgs }
    }

    case 'pickup': {
      const pickup = compositionToMsgs(beat, bpm, velocity, startTime, program, composition.pickup)
      const pickupDuration = pickup.endTime - startTime
      let notes: { endTime: number, msgs: Msg[] } | undefined
      // If the pickup would start in negative time, then rebase the composition to start
      // with the pickup instead.
      if (startTime - pickupDuration < 0) {
        pickup.msgs.forEach(msg => {
          msg.time += pickupDuration
        })
        notes = compositionToMsgs(beat, bpm, velocity, pickupDuration, program, composition.notes)

      // Otherwise, rebase pickup to start before the composition.
      } else {
        pickup.msgs.forEach(msg => {
          msg.time -= pickupDuration
        })
        notes = compositionToMsgs(beat, bpm, velocity, startTime, program, composition.notes)
      }
      const msgs: Msg[] = []
      msgs.push(...pickup.msgs)
      msgs.push(...notes.msgs)
      return { endTime: notes.endTime, msgs }
    }

    case 'mod': {
      if (composition.mod.type === 'percussion') {
        return compositionToMsgs(beat, bpm, velocity, startTime, 9, composition.note)
      } else if (composition.mod.type === 'pitchBend') {
        const msgs = []
        const data = compositionToMsgs(beat, bpm, velocity, startTime, program, composition.note)
        msgs.push(midiMsg(startTime, JZZ.MIDI.pitchBendF(0, composition.mod.fields[0])))
        msgs.push(...data.msgs)
        msgs.push(midiMsg(data.endTime, JZZ.MIDI.pitchBendF(0, 0)))
        return { msgs, endTime: data.endTime }
      } else if (composition.mod.type === 'tempo') {
        return compositionToMsgs(composition.mod.fields[0], composition.mod.fields[1], velocity, startTime, program, composition.note)
      } else if (composition.mod.type === 'dynamics') {
        return compositionToMsgs(beat, bpm, composition.mod.fields[0], startTime, program, composition.note)
      } else if (composition.mod.type === 'instrument') {
        // TODO: need to add an additional argument to compositionToMsgs
        // to pass the "current" voice. Or do we need to add infrastructure to
        // map one new voice to each channel? Need to check if channels 0-8 are
        // actually available for playback...
        console.log(composition.mod.fields[0])
        return compositionToMsgs(beat, bpm, velocity, startTime, composition.mod.fields[0], composition.note)
      } else {
        throw new ICE('compositionToMsgs', `unknown mod tag: ${composition.mod}`)
      }
    }
  }
}

export function playComposition (env: L.Env, composition: Composition): number {
  const synth: Synth = JZZ.synth.Tiny()
  const startTime = window.performance.now()
  const msgs = compositionToMsgs(dur(1, 4), 120, 64, 0, 0, composition).msgs
  let i = 0
  // NOT WORKING, I WONDER WHY!?
  for (let i = 0; i < testProgramMap.length; i++) {
    console.log(`Setting ${i} to ${testProgramMap[i]}`)
    JZZ.MIDI.program(i, testProgramMap[i])
  }
  // JZZ.synth.Tiny.setSynth(0, 50)
  // eslint-disable-next-line @typescript-eslint/no-misused-promises
  const id = window.setInterval(async () => {
    const now = window.performance.now()
    while (i < msgs.length) {
      const msg = msgs[i]
      if (msg.time + startTime <= now) {
        if (msg.tag === 'midi') {
          synth.send(msg.data)
        } else if (msg.tag === 'trigger') {
          const result = await evaluateExp(env, L.nlecall(L.nlevalue(msg.callback), []))
          console.log(result)
        }
        i += 1
        continue // N.B., try to process the next message
      } else {
        return Promise.resolve(undefined) // N.B., wait for the next interval to process msgs again
      }
    }
    clearInterval(id)
    return Promise.resolve(undefined)
  })
  return id
}
