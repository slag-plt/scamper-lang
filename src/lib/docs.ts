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

export const lt: Doc = new Doc(
  '(< v1 v2): boolean?', [
    'v1: number?',
    'v2: number?',
  ],
  'Returns `#t` if and only `v1` is strictly less than `v2`.'
)

export const leq: Doc = new Doc(
  '(<= v1 v2): boolean?', [
    'v1: number?',
    'v2: number?',
  ],
  'Returns `#t` if and only `v1` is less than or equal to `v2`.'
)

export const gt: Doc = new Doc(
  '(> v1 v2): boolean?', [
    'v1: number?',
    'v2: number?',
  ],
  'Returns `#t` if and only `v1` is strictly greater than `v2`.'
)

export const geq: Doc = new Doc(
  '(>= v1 v2): boolean?', [
    'v1: number?',
    'v2: number?',
  ],
  'Returns `#t` if and only `v1` is greater than or equal to `v2`.'
)

export const numeq: Doc = new Doc(
  '(= v1 v2): boolean?', [
    'v1: number?',
    'v2: number?',
  ],
  'Returns `#t` if and only `v1` is equal to `v2`.'
)

export const zero: Doc = new Doc(
  '(zero? v): boolean?', [
    'v: any',
  ],
  'Returns `#t` if and only `v` is zero.'
)

export const positive: Doc = new Doc(
  '(positive? v): boolean?', [
    'v: any',
  ],
  'Returns `#t` if and only `v` is positive.'
)

export const negative: Doc = new Doc(
  '(negative? v): boolean?', [
    'v: any',
  ],
  'Returns `#t` if and only `v` is negative.'
)

export const odd: Doc = new Doc(
  '(odd? v): boolean?', [
    'v: any',
  ],
  'Returns `#t` if and only `v` is odd.'
)

export const even: Doc = new Doc(
  '(even? v): boolean?', [
    'v: any',
  ],
  'Returns `#t` if and only `v` is even.'
)

export const max: Doc = new Doc(
  '(max v1 v2 ...): number?', [
    'v: number?',
  ],
  'Returns the maximum of the given numbers.'
)

export const min: Doc = new Doc(
  '(min v1 v2 ...): number?', [
    'v: number?',
  ],
  'Returns the minimum of the given numbers.'
)

export const plus: Doc = new Doc(
  '(+ v1 v2 ...): number?', [
    'v: number?',
  ],
  'Returns the sum of `v1`, `v2`, ... .'
)


export const minus: Doc = new Doc(
  '(- v1 v2 ...): number?', [
    'v: number?',
  ],
  'Returns the difference of `v1`, `v2`, ... .'
)

export const times: Doc = new Doc(
  '(* v1 v2 ...): number?', [
    'v: number?',
  ],
  'Returns the product of `v1`, `v2`, ... .'
)

export const div: Doc = new Doc(
  '(/ v1 v2 ...): number?', [
    'v: number?',
  ],
  'Returns the quotient of `v1`, `v2`, ... .'
)

export const abs: Doc = new Doc(
  '(abs v): number?', [
    'v: number?',
  ],
  'Returns the absolute value of `v`.'
)

export const modulo: Doc = new Doc(
  '(modulo v1 v2): number?', [
    'v1: number?',
    'v2: number?',
  ],
  'Returns the remainder of `v1` divided by `v2`.'
)

export const floor: Doc = new Doc(
  '(floor v): integer?', [
    'v: number?',
  ],
  'Returns the largest integer less than or equal to `v`.'
)

export const ceiling: Doc = new Doc(
  '(ceiling v): integer?', [
    'v: number?',
  ],
  'Returns the smallest integer greater than or equal to `v`.'
)

export const truncate: Doc = new Doc(
  '(truncate v): integer?', [
    'v: number?',
  ],
  'Returns the integer closest to `v` less than or equal to `v`.'
)

export const round: Doc = new Doc(
  '(round v): integer?', [
    'v: number?',
  ],
  'Returns the integer closest to `v`.'
)

export const square: Doc = new Doc(
  '(square v): number?', [
    'v: number?',
  ],
  'Returns the square of `v`.'
)

export const sqrt: Doc = new Doc(
  '(sqrt v): number?', [
    'v: number?',
  ],
  'Returns the square root of `v`.'
)

export const expt: Doc = new Doc(
  '(expt x y): number?', [
    'x: number?',
    'y: number?',
  ],
  'Returns `x` raised to the power of `y`.'
)

export const numberString: Doc = new Doc(
  '(number->string v): string?', [
    'v: number?',
  ],
  'Returns the string representation of `v`.'
)

export const not: Doc = new Doc(
  '(not v): boolean?', [
    'v: any',
  ],
  'Returns `#t` if and only `v` is `#f`.'
)

export const boolean: Doc = new Doc(
  '(boolean? v): boolean?', [
    'v: any',
  ],
  'Returns `#t` if and only `v` is a boolean.'
)

export const pair: Doc = new Doc(
  '(pair? v): boolean?', [
    'v: any',
  ],
  'Returns `#t` if and only `v` is a pair.'
)

export const cons: Doc = new Doc(
  '(cons v1 v2): pair?', [
    'v1: any',
    'v2: any',
  ],
  'Returns a new pair containing `v1` and `v2`.'
)

export const car: Doc = new Doc(
  '(car v): any', [
    'v: pair?',
  ],
  'Returns the first element of `v`.'
)

export const cdr: Doc = new Doc(
  '(cdr v): any', [
    'v: pair?',
  ],
  'Returns the second element of `v`.'
)

export const nullQ: Doc = new Doc(
  '(null? v): boolean?', [
    'v: any',
  ],
  'Returns `#t` if and only `v` is the empty list.'
)

export const listQ: Doc = new Doc(
  '(list? v): boolean?', [
    'v: any',
  ],
  'Returns `#t` if and only `v` is a list.'
)

export const list: Doc = new Doc(
  '(list v1 v2 ...): list?', [
    'v: any',
  ],
  'Returns a new list containing `v1`, `v2`, ... .'
)

export const makeList: Doc = new Doc(
  '(make-list n v): list?', [
    'n: integer?',
    'v: any',
  ],
  'Returns a new list containing `n` copies of `v`.'
)

export const length: Doc = new Doc(
  '(length v): integer?', [
    'v: list?',
  ],
  'Returns the length of `v`.'
)

export const append: Doc = new Doc(
  '(append l1 l2 ...): list?', [
    'l: list?',
  ],
  'Returns a new list containing the elements of lists `l1`, `l2`, ... in sequence.'
)

export const stringQ: Doc = new Doc(
  '(string? v): boolean?', [
    'v: any',
  ],
  'Returns `#t` if and only `v` is a string.'
)

export const stringLength: Doc = new Doc(
  '(string-length v): integer?', [
    'v: string?',
  ],
  'Returns the length of `v`.'
)

export const stringRef: Doc = new Doc(
  '(string-ref s n): string?', [
    's: string?',
    'n: integer?',
  ],
  'Returns the character at index `n` of string `s`.'
)

export const procedure: Doc = new Doc(
  '(procedure? v): boolean?', [
    'v: any',
  ],
  'Returns `#t` if and only `v` is a procedure.'
)

export const apply: Doc = new Doc(
  '(apply f v1 v2 ...): any', [
    'f: procedure?',
    'v: any',
  ],
  'Calls `f` with arguments `v1`, `v2`, ... .'
)

export const map: Doc = new Doc(
  '(map f l): list?', [
    'f: procedure?',
    'l: list?',
  ],
  'Returns a new list containing the results of applying `f` to each element of `l`.'
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