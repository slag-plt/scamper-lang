import { join } from 'path'
import * as L from '../lang.js'
import { msg } from '../messages.js'
import { ok } from '../result.js'
import { runtimeError } from '../runtime.js'
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

export type Note = { tag: 'note', pitch: PitchClass, octave: Octave, duration: Duration }
const note = (pitch: PitchClass, octave: Octave, duration: Duration): Note => ({
  tag: 'note', pitch, octave, duration
})

type Rest = { tag: 'rest', duration: Duration }
const rest = (duration: Duration): Rest => ({ tag: 'rest', duration })

type Par = { tag: 'par', notes: Composition[] }
const par = (notes: Composition[]): Par => ({ tag: 'par', notes })

type Seq = { tag: 'seq', notes: Composition[] }
const seq = (notes: Composition[]): Seq => ({ tag: 'seq', notes })

type ModKind = Instrument | PitchBend | Tempo

type Instrument = { tag: 'instrument', name: string }
const instrument = (name: string): Instrument => ({ tag: 'instrument', name })
/*const instrumentPrim: L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('instrument', ['string'], undefined, args, app).andThen(_ =>
    )*/


type PitchBend = { tag: 'pitchBend', amount: number }
const pitchBend = (amount: number): PitchBend => ({ tag: 'pitchBend', amount })
const pitchBendPrim: L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('bend', ['number?'], undefined, args, app).andThen(_ => {
    const amount = L.asNum_(args[0])
    if (amount < -1 || amount > 1) {
      return runtimeError(msg('error-precondition-not-met', 'bend', 1, '-1 <= amount <= 1', Pretty.expToString(0, args[0])), app)
    } else {
      return ok(L.nleobj('Mod', pitchBend(amount)))
    }
  })

type Tempo = { tag: 'tempo', beat: Duration, bpm: number }
const tempo = (beat: Duration, bpm: number): Tempo => ({ tag: 'tempo', beat, bpm })
const tempoPrim: L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('tempo', ['Duration', 'number?'], undefined, args, app).andThen(_ => {
    const beat = L.fromObj_<Duration>(args[0])
    const value = L.asNum_(args[1])
    return value < 0
      ? runtimeError(msg('error-precondition-not-met', 'tempo', 1, 'tempo >= 0', Pretty.expToString(0, args[1])), app)
      : ok(L.nleobj('Mod', tempo(beat, value)))
  })

/*
The Mod datatype from HSM:

| Tempo Rational -- scale the tempo
| Transpose AbsPitch -- transposition
| Instrument InstrumentName -- instrument label
| Phrase [PhraseAttribute ] -- phrase attributes
| Player PlayerName -- player label
*/

export type Mod = { tag: 'mod', note: Composition, mod: ModKind }
export const mod = (mod: ModKind, note: Composition): Mod => ({ tag: 'mod', note, mod })
export const modPrim: L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('mod', ['Mod', 'Composition'], undefined, args, app)
    .andThen(_ => ok(L.nleobj(
      'Composition',
      mod(
        L.fromObj_<ModKind>(args[0]),
        L.fromObj_<Composition>(args[1])))))

export type Composition = Note | Rest | Par | Seq | Mod

const pitchPrim: L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('pitch?', ['any'], undefined, args, app).andThen(_ =>
    ok(L.nlebool(L.isString(args[0]) && isPitchClass(L.asString_(args[0])))))

const octavePrim: L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('octave?', ['any'], undefined, args, app).andThen(_ =>
    ok(L.nlebool(L.isInteger(args[0]) && isOctave(L.asNum_(args[0])))))

const durQPrim: L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('dur?', ['any'], undefined, args, app).andThen(_ =>
    ok(L.nlebool(args[0].tag == 'obj' && args[0].kind == 'dur')))

const durPrim: L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('dur', ['number?', 'number?'], undefined, args, app).andThen(_ =>
    ok(L.nleobj('Duration', {
      num: L.asNum_(args[0]),
      den: L.asNum_(args[1])
    })))

const notePrim: L.Prim = (_env, args, app) => {
  const argErr = Utils.checkArgs('note', ['string?', 'integer?', 'Duration'], undefined, args, app)
  if (argErr) { return argErr }
  const pitch = L.asString_(args[0])
  const octave = L.asNum_(args[1])
  const dur = L.fromObj_<Duration>(args[2])
  if (!isPitchClass(pitch)) {
    return runtimeError(msg('error-type-expected-fun', 1, 'note', 'pitch', args[0].tag), app)
  } else if  (!isOctave(octave)) {
    return runtimeError(msg('error-type-expected-fun', 1, 'note', 'octave', args[1].tag), app)
  } else {
    return ok(L.nleobj('Composition', note(pitch, octave, dur)))
  }
}

const restPrim: L.Prim = (_env, args, app) => {
  const argErr = Utils.checkArgs('rest', ['Duration'], undefined, args, app)
  if (argErr) { return argErr }
  const dur = L.fromObj_<Duration>(args[0])
  return ok(L.nleobj('Composition', rest(dur)))
}

const parPrim: L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('par', [], 'Composition', args, app).andThen(_ =>
    ok(L.nleobj('Composition', par(args.map(e => L.fromObj_<Composition>(e))))))

const seqPrim: L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('seq', [], 'Composition', args, app).andThen(_ =>
    ok(L.nleobj('Composition', seq(args.map(e => L.fromObj_<Composition>(e))))))

const musicEntry = (prim: L.Prim, docs?: L.Doc) => L.entry(L.nleprim(prim), 'music', undefined, docs)

export const musicLib: L.Env = new L.Env([
  ['pitch?', musicEntry(pitchPrim, Docs.pitch)],
  ['octave?', musicEntry(octavePrim, Docs.octave)],
  ['dur?', musicEntry(durQPrim, Docs.durQ)],
  ['dur', musicEntry(durPrim, Docs.dur)],
  ['note', musicEntry(notePrim, Docs.note)],
  ['rest', musicEntry(restPrim, Docs.rest)],
  ['par', musicEntry(parPrim, Docs.par)],
  ['seq', musicEntry(seqPrim, Docs.seq)],
  ['mod', musicEntry(modPrim, Docs.mod)],
  ['bend', musicEntry(pitchBendPrim, Docs.bend)],
  ['tempo', musicEntry(tempoPrim, Docs.tempo)]
])