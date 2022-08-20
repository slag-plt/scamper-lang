import * as L from '../lang.js'
import { msg } from '../messages.js'
import { ok } from '../result.js'
import { runtimeError } from '../runtime.js'
import * as Docs from './docs.js'

type PitchClass = string
type Octave = number
type Duration = { num: number, den: number }

const isPitchClass = (s: string): boolean =>
    /^[A-Ga-g][#b]{0,2}$/.test(s)

const isOctave = (n: number): boolean =>
  n >= 0 && n <= 10

type Note = { tag: 'note', pitch: PitchClass, octave: Octave, duration: Duration }
const note = (pitch: PitchClass, octave: Octave, duration: Duration): Note => ({
  tag: 'note', pitch, octave, duration
})

type Rest = { tag: 'rest', duration: Duration }
const rest = (duration: Duration): Rest => ({ tag: 'rest', duration })

type Par = { tag: 'par', notes: Composition[] }
const par = (notes: Composition[]): Par => ({ tag: 'par', notes })

type Seq = { tag: 'seq', notes: Composition[] }
const seq = (notes: Composition[]): Seq => ({ tag: 'seq', notes })

type ModKind = void

/*
The Mod datatype from HSM:

| Tempo Rational -- scale the tempo
| Transpose AbsPitch -- transposition
| Instrument InstrumentName -- instrument label
| Phrase [PhraseAttribute ] -- phrase attributes
| Player PlayerName -- player label
*/

type Mod = { tag: 'mod', note: Composition, mod: ModKind }
const mod = (note: Composition, mod: ModKind): Mod => ({ tag: 'mod', note, mod })

type Composition = Note | Rest | Par | Seq | Mod

const pitchPrim: L.Prim = (_env, args, app) =>
  args.length !== 1
    ? runtimeError(msg('error-arity', 'pitch?', 1, args.length), app)
    : ok(L.nlebool(L.isString(args[0]) && isPitchClass(L.asString_(args[0]))))

const octavePrim: L.Prim = (_env, args, app) =>
  args.length !== 1
    ? runtimeError(msg('error-arity', 'octave?', 1, args.length), app)
    : ok(L.nlebool(L.isInteger(args[0]) && isOctave(L.asNum_(args[0]))))

const durQPrim: L.Prim = (_env, args, app) =>
  args.length !== 1
    ? runtimeError(msg('error-arity', 'dur?', 1, args.length), app)
    : ok(L.nlebool(args[0].tag == 'obj' && args[0].kind == 'dur'))

const durPrim: L.Prim = (_env, args, app) => {
  if (args.length !== 2) {
    return runtimeError(msg('error-arity', 'dur', 2, args.length), app)
  } else if (!L.isInteger(args[0])) {
    return runtimeError(msg('error-type-expected-fun', 'dur', 'integer', args[0].tag), app)
  } else if (!L.isInteger(args[1])) {
    return runtimeError(msg('error-type-expected-fun', 'dur', 'integer', args[1].tag), app)
  } else {
    return ok(L.nleobj('Duration', {
      num: L.asNum_(args[0]),
      den: L.asNum_(args[1])
    }))
  }
}

const notePrim: L.Prim = (_env, args, app) => {
  if (args.length !== 3) {
    return runtimeError(msg('error-arity', 'note', 3, args.length), app)
  } else if (!L.isString(args[0])) {
    return runtimeError(msg('error-type-expected-fun', 'note', 'string', args[0]), app)
  } else if (!L.isInteger(args[1])) {
    return runtimeError(msg('error-type-expected-fun', 'note', 'integer', args[0]), app)
  } else if (!L.isObjKind(args[2], 'Duration')) {
    return runtimeError(msg('error-type-expected-fun', 'note', 'duration', args[0]), app)
  } else {
    const pitch = L.asString_(args[0])
    const octave = L.asNum_(args[1])
    const dur = L.fromObj_<Duration>(args[2])
    if (!isPitchClass(pitch)) {
      return runtimeError(msg('error-type-expected-fun', 'note', 'pitch', args[0].tag), app)
    } else if  (!isOctave(octave)) {
      return runtimeError(msg('error-type-expected-fun', 'note', 'octave', args[1].tag), app)
    } else {
      return ok(L.nleobj('Composition', note(pitch, octave, dur)))
    }
  }
}

const restPrim: L.Prim = (_env, args, app) => {
  if (args.length !== 1) {
    return runtimeError(msg('error-arity', 'rest', 1, args.length), app)
  } else if (!L.isObjKind(args[0], 'Duration')) {
    return runtimeError(msg('error-type-expected-fun', 'rest', 'duration', args[0]), app)
  } else {
    const dur = L.fromObj_<Duration>(args[2])
    return ok(L.nleobj('Composition', rest(dur)))
  }
}

const parPrim: L.Prim = (_env, args, app) => {
  args.forEach(e => {
    if (!L.isObjKind(e, 'Composition')) {
      return runtimeError(msg('error-type-expected-fun', 'par', 'composition', e.tag), app)
    }
  })
  return ok(L.nleobj('Composition', par(args.map(e => L.fromObj_<Composition>(e)))))
}

const seqPrim: L.Prim = (_env, args, app) => {
  args.forEach(e => {
    if (!L.isObjKind(e, 'Composition')) {
      return runtimeError(msg('error-type-expected-fun', 'par', 'composition', e.tag), app)
    }
  })
  return ok(L.nleobj('Composition', seq(args.map(e => L.fromObj_<Composition>(e)))))
}

const musicEntry = (prim: L.Prim, docs?: L.Doc) => L.entry(L.nleprim(prim), 'music', undefined, docs)

export const musicLib: L.Env = new L.Env([
  ['pitch?', musicEntry(pitchPrim, Docs.pitch)],
  ['octave?', musicEntry(octavePrim)],
  ['dur?', musicEntry(durQPrim)],
  ['dur', musicEntry(durPrim)],
  ['note', musicEntry(notePrim)],
  ['rest', musicEntry(restPrim)],
  ['par', musicEntry(parPrim)],
  ['seq', musicEntry(seqPrim)]
])