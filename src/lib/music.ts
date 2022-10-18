/* eslint-disable no-use-before-define */
import * as L from '../lang.js'
import { msg } from '../messages.js'
import { ok } from '../result.js'
import { evaluateExp, runtimeError } from '../runtime.js'
import * as Docs from './docs.js'
import * as Pretty from '../pretty.js'
import * as Utils from './utils.js'

export type PitchClass = string
export type Octave = number
export type Duration = { num: number, den: number }

const isPitchClass = (s: string): boolean =>
  /^[A-Ga-g][#b]{0,2}$/.test(s)

const isOctave = (n: number): boolean =>
  n >= 0 && n <= 10

const isValidMidiNote = (n: number): boolean =>
  n >= 0 && n <= 127

export type Note = { tag: 'note', note: number, duration: Duration }
const note = (note: number, duration: Duration): Note => ({
  tag: 'note', note, duration
})

const repeatPrim: L.Prim = async (env, args, app) =>
  Utils.checkArgsResult('repeat', ['integer?', 'Composition'], undefined, args, app).asyncAndThen(async _ => {
    const n = L.asNum_(args[0])
    if (n < 0) {
      return runtimeError(msg('error-precondition-not-met', 'repeat', 1, 'non-negative integer', Pretty.expToString(0, args[0])))
    } else {
      return evaluateExp(env,
        L.nleif(
          L.nlecall(L.nlevar('='), [args[0], L.nlenumber(0)]),
          L.nlevar('empty'),
          L.nlecall(L.nlevar('seq'), [
            args[1],
            L.nlecall(L.nlevar('repeat'), [
              L.nlecall(L.nlevar('-'), [args[0], L.nlenumber(1)]),
              args[1]
            ])
          ])
        )
      )
    }
  })

type Empty = { tag: 'empty' }
const empty = (): Empty => ({ tag: 'empty' })

type Rest = { tag: 'rest', duration: Duration }
const rest = (duration: Duration): Rest => ({ tag: 'rest', duration })

type Par = { tag: 'par', notes: Composition[] }
const par = (notes: Composition[]): Par => ({ tag: 'par', notes })

type Seq = { tag: 'seq', notes: Composition[] }
const seq = (notes: Composition[]): Seq => ({ tag: 'seq', notes })

type Pickup = { tag: 'pickup', pickup: Composition, notes: Composition }
const pickup = (pickup: Composition, notes: Composition): Composition => ({ tag: 'pickup', pickup, notes })

type ModKind = Percussion | PitchBend | Tempo | Dynamics
type Percussion = { tag: 'percussion' }
const percussion = (): Percussion => ({ tag: 'percussion' })

type PitchBend = { tag: 'pitchBend', amount: number }
const pitchBend = (amount: number): PitchBend => ({ tag: 'pitchBend', amount })
const pitchBendPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('bend', ['number?'], undefined, args, app).andThen(_ => {
    const amount = L.asNum_(args[0])
    if (amount < -1 || amount > 1) {
      return runtimeError(msg('error-precondition-not-met', 'bend', 1, '-1 <= amount <= 1', Pretty.expToString(0, args[0])), app)
    } else {
      return ok(L.nlestruct('Mod', pitchBend(amount)))
    }
  }))

type Tempo = { tag: 'tempo', beat: Duration, bpm: number }
const tempo = (beat: Duration, bpm: number): Tempo => ({ tag: 'tempo', beat, bpm })
const tempoPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('tempo', ['Duration', 'number?'], undefined, args, app).andThen(_ => {
    const beat = L.fromStruct_<Duration>(args[0])
    const value = L.asNum_(args[1])
    return value < 0
      ? runtimeError(msg('error-precondition-not-met', 'tempo', 1, 'tempo >= 0', Pretty.expToString(0, args[1])), app)
      : ok(L.nlestruct('Mod', tempo(beat, value)))
  }))

type Dynamics = { tag: 'dynamics', amount: number }
const dynamics = (amount: number): Dynamics => ({ tag: 'dynamics', amount })
const dynamicsPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('dynamics', ['number?'], undefined, args, app).andThen(_ => {
    const amount = L.asNum_(args[0])
    if (amount < 0 || amount > 127) {
      return runtimeError(msg('error-precondition-not-met', 'dynamics', 1, '0 <= amount <= 127', Pretty.expToString(0, args[0])), app)
    } else {
      return ok(L.nlestruct('Mod', dynamics(amount)))
    }
  }))

export type Mod = { tag: 'mod', note: Composition, mod: ModKind }
export const mod = (mod: ModKind, note: Composition): Mod => ({ tag: 'mod', note, mod })
export const modPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('mod', ['Mod', 'Composition'], undefined, args, app)
    .andThen(_ => ok(L.nlestruct(
      'Composition',
      mod(
        L.fromStruct_<ModKind>(args[0]),
        L.fromStruct_<Composition>(args[1]))))))

export type Composition = Empty | Note | Rest | Par | Seq | Pickup | Mod

const pitchQPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('pitch?', ['any'], undefined, args, app).andThen(_ =>
    ok(L.nlebool(L.isString(args[0]) && isPitchClass(L.asString_(args[0]))))))

const octavePrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('octave?', ['any'], undefined, args, app).andThen(_ =>
    ok(L.nlebool(L.isInteger(args[0]) && isOctave(L.asNum_(args[0]))))))

const durQPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('dur?', ['any'], undefined, args, app).andThen(_ =>
    ok(L.nlebool(args[0].tag === 'struct' && args[0].kind === 'dur'))))

const durPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('dur', ['number?', 'number?'], undefined, args, app).andThen(_ =>
    ok(L.nlestruct('Duration', {
      num: L.asNum_(args[0]),
      den: L.asNum_(args[1])
    }))))

const notePrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('note', ['integer?', 'Duration'], undefined, args, app).andThen(_ => {
    const midiNote = L.asNum_(args[0])
    const dur = L.fromStruct_<Duration>(args[1])
    if (!isValidMidiNote(midiNote)) {
      return runtimeError(msg('error-precondition-not-met', 'note', 1,
        '0 <= amount <= 128', Pretty.expToString(0, args[0])), app)
    } else {
      return ok(L.nlestruct('Composition', note(midiNote, dur)))
    }
  }))

const restPrim: L.Prim = (_env, args, app) => {
  const argErr = Utils.checkArgs('rest', ['Duration'], undefined, args, app)
  if (argErr) { return Promise.resolve(argErr) }
  const dur = L.fromStruct_<Duration>(args[0])
  return Promise.resolve(ok(L.nlestruct('Composition', rest(dur))))
}

const parPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('par', [], 'Composition', args, app).andThen(_ =>
    ok(L.nlestruct('Composition', par(args.map(e => L.fromStruct_<Composition>(e)))))))

const seqPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('seq', [], 'Composition', args, app).andThen(_ =>
    ok(L.nlestruct('Composition', seq(args.map(e => L.fromStruct_<Composition>(e)))))))

const pickupPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('pickup', ['Composition', 'Composition'], undefined, args, app).andThen(_ =>
    ok(L.nlestruct('Composition', pickup(L.fromStruct_<Composition>(args[0]), L.fromStruct_<Composition>(args[1]))))))

const numeratorPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('numerator', ['Duration'], undefined, args, app).andThen(_ =>
    ok(L.nlenumber(L.fromStruct_<Duration>(args[0]).num))))

const denominatorPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('denominator', ['Duration'], undefined, args, app).andThen(_ =>
    ok(L.nlenumber(L.fromStruct_<Duration>(args[0]).den))))

const musicEntry = (prim: L.Prim, docs?: L.Doc) => L.entry(L.nleprim(prim), 'music', undefined, docs)

export const musicLib: L.Env = new L.Env([
  ['pitch?', musicEntry(pitchQPrim, Docs.pitch)],
  ['octave?', musicEntry(octavePrim, Docs.octave)],
  ['dur?', musicEntry(durQPrim, Docs.durQ)],
  ['dur', musicEntry(durPrim, Docs.dur)],
  ['numerator', musicEntry(numeratorPrim, Docs.numerator)],
  ['denominator', musicEntry(denominatorPrim, Docs.denominator)],
  ['empty', L.entry(L.nlestruct('Composition', empty()), 'music', undefined, Docs.empty)],
  ['note', musicEntry(notePrim, Docs.note)],
  ['rest', musicEntry(restPrim, Docs.rest)],
  ['par', musicEntry(parPrim, Docs.par)],
  ['seq', musicEntry(seqPrim, Docs.seq)],
  ['pickup', musicEntry(pickupPrim, Docs.pickup)],
  ['mod', musicEntry(modPrim, Docs.mod)],
  ['percussion', L.entry(L.nlestruct('Mod', percussion()), 'music', undefined, Docs.percussion)],
  ['bend', musicEntry(pitchBendPrim, Docs.bend)],
  ['tempo', musicEntry(tempoPrim, Docs.tempo)],
  ['dynamics', musicEntry(dynamicsPrim, Docs.dynamics)],
  ['repeat', musicEntry(repeatPrim, Docs.repeat)],
  ['wn', L.entry(L.nlestruct('Duration', { num: 1, den: 1 }), 'music', undefined, Docs.wn)],
  ['hn', L.entry(L.nlestruct('Duration', { num: 1, den: 2 }), 'music', undefined, Docs.hn)],
  ['qn', L.entry(L.nlestruct('Duration', { num: 1, den: 4 }), 'music', undefined, Docs.qn)],
  ['en', L.entry(L.nlestruct('Duration', { num: 1, den: 8 }), 'music', undefined, Docs.en)],
  ['sn', L.entry(L.nlestruct('Duration', { num: 1, den: 16 }), 'music', undefined, Docs.sn)],
  ['tn', L.entry(L.nlestruct('Duration', { num: 1, den: 32 }), 'music', undefined, Docs.tn)]
])
