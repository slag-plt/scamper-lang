import { Doc } from '../lang.js'

///// Prelude //////////////////////////////////////////////////////////////////

export const equal: Doc = new Doc(
  '(equal? v1 v2): boolean?', [
    'v1: any',
    'v2: any',
  ],
  'Returns `#t` if and only `v1` and `v2` are (structurally) equal values.'
)

export const number: Doc = new Doc(
  '(number? v): boolean?', [
    'v: any'
  ],
  'Returns `#t` if and only `v` is a number.'
)

export const real: Doc = new Doc(
  '(real? v): boolean?', [
    'v: any'
  ],
  'Returns `#t` if and only `v` is a real number.'
)

export const integer: Doc = new Doc(
  '(integer? v): boolean?', [
    'v: any'
  ],
  'Returns `#t` if and only `v` is an integer.'
)

///// image ////////////////////////////////////////////////////////////////////

export const circle: Doc = new Doc(
  '(circle radius fill color): drawing?', [
    'radius: number?',
    'fill: string?, either "solid" or "outline"',
    'color: string? of the form "rgba(r, g, b, a)"'
  ],
  'Returns a drawing consisting of a circle of radius `radius`.'
)

export const rectangle: Doc = new Doc(
  '(rectangle width height fill color): drawing?', [
    'width: number?',
    'height: number?',
    'fill: string?, either "solid" or "outline"',
    'color: string? of the form "rgba(r, g, b, a)"'
  ],
  'Returns a drawing consisting of a rectangle with dimension `width × height`.'
)

export const beside: Doc = new Doc(
  '(beside d1 d2 ...): drawing?', [
    'd: drawing?'
  ],
  'Creates a new drawing formed by places the drawing `d1`, `d2`, ..., beside each other (horizontally).'
)

export const above: Doc = new Doc(
  '(above d1 d2 ...): drawing?', [
    'd: drawing?'
  ],
  'Creates a new drawing formed by places the drawing `d1`, `d2`, ..., above each other (vertically in descending order).'
)

export const overlay: Doc = new Doc(
  '(overlay d1 d2 ...): drawing?', [
    'd: drawing?'
  ],
  'Creates a new drawing formed by places the drawing `d1`, `d2`, ..., on top of each other. (`d1` is the topmost drawing).'
)

///// music ////////////////////////////////////////////////////////////////////

export const pitch: Doc = new Doc(
  '(pitch? v): boolean?', [
    'v: any'
  ],
  'Returns `#t` if and only `v` is a valid pitch, a string denoting a pitch class, e.g., `"Ab"`.'
)

export const octave: Doc = new Doc(
  '(octave? v): boolean?', [
    'v: any'
  ],
  'Returns `#t` if and only `v` is a valid octave, an integer in the range (0, 10).'
)

export const durQ: Doc = new Doc(
  '(dur? v): boolean?', [
    'v: any'
  ],
  'Returns `#t` if and only `v` is a valid duration object.'
)

export const dur: Doc = new Doc(
  '(dur num den): duration?', [
    'num: integer?',
    'den: integer?'
  ],
  'Creates a new duration object representing the ratio `num/den`.'
)

export const note: Doc = new Doc(
  '(note pit oct dur): composition?', [
    'pit: pitch?',
    'oct: integer?',
    'dur: dur?'
  ],
  'Creates a new composition consisting of a single note from the given pitch, octave, and duration.'
)

export const rest: Doc = new Doc(
  '(rest dur): composition?', [
    'dur: dur?'
  ],
  'Creates a new composition consisting of a single rest from the given duration.'
)

export const par: Doc = new Doc(
  '(par comp1 comp2 ...): composition?', [
    'comp: composition?',
  ],
  'Creates a new composition that plays `comp1`, `comp2`, ..., in parallel.'
)

export const seq: Doc = new Doc(
  '(seq comp1 comp2 ...): composition?', [
    'comp: composition?',
  ],
  'Creates a new composition that plays `comp1`, `comp2`, ..., in sequence.'
)