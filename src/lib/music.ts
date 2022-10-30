/* eslint-disable no-use-before-define */
import * as L from '../lang.js'
import { msg } from '../messages.js'
import { ok } from '../result.js'
import { runtimeError } from '../runtime.js'
import { evalExp } from '../evaluator.js'
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
      return evalExp(env,
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

type Par = { renderAs: 'composition', tag: 'par', notes: Composition[] }
const par = (notes: Composition[]): Par => ({ renderAs: 'composition', tag: 'par', notes })

type Seq = { renderAs: 'composition', tag: 'seq', notes: Composition[] }
const seq = (notes: Composition[]): Seq => ({ renderAs: 'composition', tag: 'seq', notes })

type Pickup = { renderAs: 'composition', tag: 'pickup', pickup: Composition, notes: Composition }
const pickup = (pickup: Composition, notes: Composition): Composition =>
  ({ renderAs: 'composition', tag: 'pickup', pickup, notes })

type ModKind = Percussion | PitchBend | Tempo | Dynamics
type Percussion = { _scamperTag: 'struct', kind: 'percussion', fields: []}
const percussion = (): Percussion => ({ _scamperTag: 'struct', kind: 'percussion', fields: [] })

type PitchBend = { _scamperTag: 'struct', kind: 'pitchBend', fields: [number] }
const pitchBend = (amount: number): PitchBend =>
  ({ _scamperTag: 'struct', kind: 'pitchBend', fields: [amount] })
const pitchBendPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('bend', ['number?'], undefined, args, app).andThen(_ => {
    const amount = args[0] as number
    if (amount < -1 || amount > 1) {
      return runtimeError(msg('error-precondition-not-met', 'bend', 1, '-1 <= amount <= 1', Pretty.expToString(0, L.nlevalue(amount)), app))
    } else {
      return ok(pitchBend(amount))
    }
  }))

type Tempo = { _scamperTag: 'struct', kind: 'tempo', fields: [Duration, number] }
const tempo = (beat: Duration, bpm: number): Tempo => ({ _scamperTag: 'struct', kind: 'tempo', fields: [beat, bpm] })
const tempoPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('tempo', ['duration', 'number?'], undefined, args, app).andThen(_ => {
    const beat = args[0] as Duration
    const value = args[1] as number
    return value < 0
      ? runtimeError(msg('error-precondition-not-met', 'tempo', 1, 'tempo >= 0', Pretty.expToString(0, L.nlevalue(args[1]))), app)
      : ok(tempo(beat, value))
  }))

type Dynamics = { _scamperTag: 'struct', kind: 'dynamics', fields: [number] }
const dynamics = (amount: number): Dynamics => ({ _scamperTag: 'struct', kind: 'dynamics', fields: [amount] })
const dynamicsPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('dynamics', ['number?'], undefined, args, app).andThen(_ => {
    const amount = args[0] as number
    if (amount < 0 || amount > 127) {
      return runtimeError(msg('error-precondition-not-met', 'dynamics', 1, '0 <= amount <= 127', Pretty.expToString(0, L.nlevalue(args[0]))), app)
    } else {
      return ok(dynamics(amount))
    }
  }))

export type Mod = { renderAs: 'composition', tag: 'mod', note: Composition, mod: ModKind }
export const mod = (mod: ModKind, note: Composition): Mod => ({ renderAs: 'composition', tag: 'mod', note, mod })
export const modPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('mod', ['mod', 'composition'], undefined, args, app)
    .andThen(_ => ok(mod(args[0] as ModKind, args[1] as Composition))))

export type Composition = Empty | Note | Rest | Par | Seq | Pickup | Mod

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

const parPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('par', [], 'composition', args, app).andThen(_ =>
    ok(par(args as Composition[]))))

const seqPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('seq', [], 'composition', args, app).andThen(_ =>
    ok(seq(args as Composition[]))))

const pickupPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('pickup', ['chmposition', 'composition'], undefined, args, app).andThen(_ =>
    ok(pickup(args[0] as Composition, args[1] as Composition))))

const numeratorPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('numerator', ['dur'], undefined, args, app).andThen(_ =>
    ok((args[0] as Duration).fields[0])))

const denominatorPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('denominator', ['dur'], undefined, args, app).andThen(_ =>
    ok((args[1] as Duration).fields[0])))

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
  ['repeat', musicEntry(repeatPrim, Docs.repeat)],
  ['wn', L.entry(dur(1, 1), 'music', undefined, Docs.wn)],
  ['hn', L.entry(dur(1, 2), 'music', undefined, Docs.hn)],
  ['qn', L.entry(dur(1, 4), 'music', undefined, Docs.qn)],
  ['en', L.entry(dur(1, 8), 'music', undefined, Docs.en)],
  ['sn', L.entry(dur(1, 16), 'music', undefined, Docs.sn)],
  ['tn', L.entry(dur(1, 32), 'music', undefined, Docs.tn)]
])
