// duration = fraction
// note = (pitch, octave, duration)
// rest = duration
// composition ::= note | rest | par composition composition | seq composition composition | modify composition

import { ICE } from '../result.js'

type Pitch = string
type Octave = number
type Duration = { numerator: number, denominator: number }

type Note = { tag: 'note', pitch: Pitch, octave: Octave, duration: Duration }
const note = (pitch: Pitch, octave: Octave, duration: Duration): Note => ({
  tag: 'note', pitch, octave, duration
})

type Rest = { tag: 'rest', duration: Duration }
const rest = (duration: Duration): Rest => ({ tag: 'rest', duration })

type Par = { tag: 'par', notes: Composition[] }
const par = (notes: Composition[]): Par => ({ tag: 'par', notes })

type Seq = { tag: 'seq', notes: Composition[] }
const seq = (notes: Composition[]): Seq => ({ tag: 'seq', notes })

type ModKind = void

type Mod = { tag: 'mod', note: Composition, mod: ModKind }
const mod = (note: Composition, mod: ModKind): Mod => ({ tag: 'mod', note, mod })

type Composition = Note | Rest | Par | Seq | Mod

function pitchToBaseMIDIValue (pitch: string): number {
  switch (pitch) {
    case 'A': return 21
    case 'As': return 22
    case 'Bf': return 22
    case 'B': return 23
    case 'C': return 24
    case 'Cs': return 25
    case 'Df': return 25
    case 'D': return 26
    case 'Ds': return 27
    case 'Ef': return 27
    case 'E': return 28
    case 'F': return 29
    case 'Fs': return 30
    case 'Gf': return 30
    case 'G': return 31
    case 'Gs': return 32
    case 'Af': return 32
    default:
      throw new ICE('pitchToBaseMIDIValue', `Unknown pitch ${pitch}`)
  }
}

export function noteToMIDIValue (note: Note): number {
  return 12 * (note.octave + 1) + pitchToBaseMIDIValue(note.pitch)
}