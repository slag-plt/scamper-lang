import { ICE } from '../result.js'

type PitchClass = string
type Octave = number
type Duration = { numerator: number, denominator: number }

const pitches = [
  'A',
  'B',
  'C',
  'D',
  'E',
  'F',
  'G'
]

const accidentals = [
  'bb',
  'b',
  '',
  '#',
  '##'
]

const pitchClasses: PitchClass[] =
  pitches.flatMap(pitch =>
    accidentals.map(accidental => `${pitch}${accidental}`))

const isOctave = (octave: number): boolean =>
  octave >= 0 && octave <= 10

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

function pitchToBaseMIDIValue (pitch: PitchClass): number {
  switch (pitch) {
    case 'A': return 21
    case 'A#': return 22
    case 'Bb': return 22
    case 'B': return 23
    case 'C': return 24
    case 'C#': return 25
    case 'Db': return 25
    case 'D': return 26
    case 'D#': return 27
    case 'Eb': return 27
    case 'E': return 28
    case 'F': return 29
    case 'F#': return 30
    case 'Gb': return 30
    case 'G': return 31
    case 'G#': return 32
    case 'Ab': return 32
    default:
      throw new ICE('pitchToBaseMIDIValue', `Unknown pitch ${pitch}`)
  }
}

export function noteToMIDIValue (note: Note): number {
  return 12 * (note.octave + 1) + pitchToBaseMIDIValue(note.pitch)
}