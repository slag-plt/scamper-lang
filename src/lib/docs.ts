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

export const nanQ: Doc = new Doc(
  '(nan? v): boolean?', [
    'v: any'
  ],
  'Returns `#t` if and only `v` is the number `NaN`.'
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

export const quotient: Doc = new Doc(
  '(quotient v1 v2): number?', [
    'v1: integer?',
    'v2: integer?',
  ],
  'Returns the quotient of `v1` and `v2`, _i.e._, the whole number part of `v1 / v2`.'
)

export const remainder: Doc = new Doc(
  '(remainder v1 v2): number?', [
    'v1: integer?',
    'v2: integer?',
  ],
  'Returns the remainder of `v1` and `v2`, _i.e._, the remainder of `v1 / v2`.'
)

export const modulo: Doc = new Doc(
  '(modulo v1 v2): number?', [
    'v1: number?',
    'v2: number?',
  ],
  'Returns \`k = n - d * q\` where `q` is the integer such that `k` has the same sign as the divisor `d` while being as close to 0 as possible. (Source: [MDN docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Remainder).)'
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

export const stringNumber: Doc = new Doc(
  '(string->number s): number?', [
    's: string?, presumed to be a number',
  ],
  'Returns the number denoted by `s` as a `number`.'
)

export const exp: Doc = new Doc(
  '(exp v): number?', [
    'v: number?',
  ],
  'Returns the exponential of `v`.'
)

export const log: Doc = new Doc(
  '(log v): number?', [
    'v: number?',
  ],
  'Returns the natural logarithm of `v`.'
)

export const sin: Doc = new Doc(
  '(sin v): number?', [
    'v: number?',
  ],
  'Returns the sine of `v`.'
)

export const cos: Doc = new Doc(
  '(cos v): number?', [
    'v: number?',
  ],
  'Returns the cosine of `v`.'
)

export const tan: Doc = new Doc(
  '(tan v): number?', [
    'v: number?',
  ],
  'Returns the tangent of `v`.'
)

export const asin: Doc = new Doc(
  '(asin v): number?', [
    'v: number?',
  ],
  'Returns the arc sine of `v`.'
)

export const acos: Doc = new Doc(
  '(acos v): number?', [
    'v: number?',
  ],
  'Returns the arc cosine of `v`.'
)

export const atan: Doc = new Doc(
  '(atan v): number?', [
    'v: number?',
  ],
  'Returns the arc tangent of `v`.'
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

export const nand: Doc = new Doc(
  '(nand v1 v2 ...): boolean?', [
    'v: boolean?',
  ],
  'Equivalent to `(not (and v1 v2 ...))`.'
)

export const nor: Doc = new Doc(
  '(nor v1 v2 ...): boolean?', [
    'v: boolean?',
  ],
  'Equivalent to `(not (or v1 v2 ...))`.'
)

export const implies: Doc = new Doc(
  '(implies v1 v2): boolean?', [
    'v1: boolean?',
    'v2: boolean?',
  ],
  'Equivalent to `(if v1 v2 #t)`.'
)

export const xor: Doc = new Doc(
  '(xor v1 v2): boolean?', [
    'v1: boolean?',
    'v2: boolean?',
  ],
  'Equivalent to `(or (and v1 (not v2)) (and (not v1) v2))`.'
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

export const reverse: Doc = new Doc(
  '(reverse l): list?', [
    'l: list?',
  ],
  'Returns a new list containing the elements of `l` in reverse order.'
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

export const stringSplit: Doc = new Doc(
  '(string-split s sep): list?', [
    's: string?',
    'sep: string?',
  ],
  'Returns a list of strings obtained by splitting `s` at occurrences of `sep`.'
)

export const stringAppend: Doc = new Doc(
  '(string-append s1 s2 ...): string?', [
    's: string?',
  ],
  'Returns a string made by joining `s1`, `s2`, ... together.'
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

export const filter: Doc = new Doc(
  '(filter f l): list?', [
    'f: procedure?, a predicate',
    'l: list?',
  ],
  'Returns a new list containing the elements of `l` for which `f` returns `#t`.'
)

export const fold: Doc = new Doc(
  '(fold f v l): any', [
    'f: procedure?, a binary function',
    'v: any',
    'l: list?',
  ],
  'Returns the result of accumulating the result of applying `f` to each element of `l`, starting with initial value `v`.'
)

export const reduce: Doc = new Doc(
  '(reduce f l): any', [
    'f: procedure?, a binary function',
    'l: list?, non-empty',
  ],
  'Like `fold` but uses the first element of `l` as the initial value.'
)

///// image ////////////////////////////////////////////////////////////////////

export const color: Doc = new Doc(
  '(color r b g a): string?', [
    'r: integer?, 0 <= r <= 255',
    'b: integer?, 0 <= b <= 255',
    'g: integer?, 0 <= g <= 255',
    'a: integer?, 0 <= a <= 1'
  ],
  'Returns a string of the form `"rgba(r, g, b, a)"` appropriate for use as a color.'
)

export const circle: Doc = new Doc(
  '(circle radius fill color): drawing?', [
    'radius: number?',
    'fill: string?, either "solid" or "outline"',
    'color: string?, either a color name or the form "rgba(r, g, b, a)"'
  ],
  'Returns a drawing consisting of a circle of radius `radius`.'
)

export const rectangle: Doc = new Doc(
  '(rectangle width height fill color): drawing?', [
    'width: number?',
    'height: number?',
    'fill: string?, either "solid" or "outline"',
    'color: string?, either a color name or the form "rgba(r, g, b, a)"'
  ],
  'Returns a drawing consisting of a rectangle with dimension `width × height`.'
)

export const drawingSquare: Doc = new Doc(
  '(square width fill color): drawing?', [
    'width: number?',
    'fill: string?, either "solid" or "outline"',
    'color: string?, either a color name or the form "rgba(r, g, b, a)"'
  ],
  'Returns a drawing consisting of a square with length `width`.'
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