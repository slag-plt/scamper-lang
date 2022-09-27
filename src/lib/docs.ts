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

export const equalsEps: Doc = new Doc(
  '(=eps n): procedure?', [
    'n: number?',
  ],
  'Returns a function that takes two numbers `x` and `y` as input returns `#t` if `|x - y| < n`.'
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

export const pairQ: Doc = new Doc(
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

export const pair: Doc = new Doc(
  '(pair v1 v2): pair?', [
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

export const listTail: Doc = new Doc(
  '(list-tail l k): list?', [
    'l: list?',
    'k: integer?, 0 <= k <= (length l)',
  ],
  'Returns a new list containing the last `k` elements of `l.'
)

export const listDrop: Doc = new Doc(
  '(list-drop l k): list?', [
    'l: list?',
    'k: integer?, 0 <= k <= (length l)',
  ],
  'An alias for `(list-tail l k)`.'
)

export const listTake: Doc = new Doc(
  '(list-take l k): list?', [
    'l: list?',
    'k: integer?, 0 <= k <= (length l)',
  ],
  'Returns a new list containing the first `k` elements of `l`.'
)

export const listRef: Doc = new Doc(
  '(list-ref l n): any', [
    'l: list?',
    'n: integer?, 0 <= n < (length l)',
  ],
  'Returns the `n`th element of `l`.'
)

export const indexOf: Doc = new Doc(
  '(index-of l v): integer?', [
    'l: list?',
    'v: any',
  ],
  'Returns the index of the first occurrence of `v` in `l` or `-1` if `v` is not in `l`.'
)

export const charQ: Doc = new Doc(
  '(char? v): boolean?', [
    'v: any',
  ],
  'Returns `#t` if and only `v` is a character.'
)

export const charEq: Doc = new Doc(
  '(char=? c1 c2 ...): boolean?', [
    'c: char?',
  ],
  'Returns `#t` if and only `c1`, `c2`, ... are all equivalent characters.'
)

export const charLt: Doc = new Doc(
  '(char<? c1 c2 ...): boolean?', [
    'c: char?',
  ],
  'Returns `#t` if and only `c1`, `c2`, ... have strictly increasing character values.'
)

export const charGt: Doc = new Doc(
  '(char>? c1 c2 ...): boolean?', [
    'c: char?',
  ],
  'Returns `#t` if and only `c1`, `c2`, ... have strictly decreasing character values.'
)

export const charLeq: Doc = new Doc(
  '(char<=? c1 c2 ...): boolean?', [
    'c: char?',
  ],
  'Returns `#t` if and only `c1`, `c2`, ... have non-decreasing character values.'
)

export const charGeq: Doc = new Doc(
  '(char>=? c1 c2 ...): boolean?', [
    'c: char?',
  ],
  'Returns `#t` if and only `c1`, `c2`, ... have non-increasing character values.'
)

export const charEqCi: Doc = new Doc(
  '(char-ci=? c1 c2 ...): boolean?', [
    'c: char?',
  ],
  'Returns `#t` if and only `c1`, `c2`, ... are all equivalent characters, ignoring case.'
)

export const charLtCi: Doc = new Doc(
  '(char-ci<? c1 c2 ...): boolean?', [
    'c: char?',
  ],
  'Returns `#t` if and only `c1`, `c2`, ... have strictly increasing character values, ignoring case.'
)

export const charGtCi: Doc = new Doc(
  '(char-ci>? c1 c2 ...): boolean?', [
    'c: char?',
  ],
  'Returns `#t` if and only `c1`, `c2`, ... have strictly decreasing character values, ignoring case.'
)

export const charLeqCi: Doc = new Doc(
  '(char-ci<=? c1 c2 ...): boolean?', [
    'c: char?',
  ],
  'Returns `#t` if and only `c1`, `c2`, ... have non-decreasing character values, ignoring case.'
)

export const charGeqCi: Doc = new Doc(
  '(char-ci>=? c1 c2 ...): boolean?', [
    'c: char?',
  ],
  'Returns `#t` if and only `c1`, `c2`, ... have non-increasing character values, ignoring case.'
)

export const charAlphabetic: Doc = new Doc(
  '(char-alphabetic? c): boolean?', [
    'c: char?',
  ],
  'Returns `#t` if and only `c` is an alphabetic character.'
)

export const charNumeric: Doc = new Doc(
  '(char-numeric? c): boolean?', [
    'c: char?',
  ],
  'Returns `#t` if and only `c` is a numeric character.'
)

export const charWhitespace: Doc = new Doc(
  '(char-whitespace? c): boolean?', [
    'c: char?',
  ],
  'Returns `#t` if and only `c` is a whitespace character.'
)

export const charUpperCase: Doc = new Doc(
  '(char-upper-case? c): boolean?', [
    'c: char?',
  ],
  'Returns `#t` if and only `c` is an upper-case character.'
)

export const charLowerCase: Doc = new Doc(
  '(char-lower-case? c): boolean?', [
    'c: char?',
  ],
  'Returns `#t` if and only `c` is a lower-case character.'
)

export const digitValue: Doc = new Doc(
  '(digit-value c): integer?', [
    'c: char?',
  ],
  'Returns the numeric value of `c` if `c` is a decimal digit (0-10), otherwise raises an error.'
)

export const charToInteger: Doc = new Doc(
  '(char->integer c): integer?', [
    'c: char?',
  ],
  'Returns the codepoint value of character `c`.'
)

export const integerToChar: Doc = new Doc(
  '(integer->char n): char?', [
    'n: integer?',
  ],
  'Returns the character with codepoint value `n`.'
)

export const charUpcase: Doc = new Doc(
  '(char-upcase c): char?', [
    'c: char?',
  ],
  'Returns the upper-case equivalent of `c`.'
)

export const charDowncase: Doc = new Doc(
  '(char-downcase c): char?', [
    'c: char?',
  ],
  'Returns the lower-case equivalent of `c`.'
)

export const charFoldcase: Doc = new Doc(
  '(char-foldcase c): char?', [
    'c: char?',
  ],
  'Returns the case-folded equivalent of `c`. This is a version of `c` that is appropriate for case-insensitive comparison.'
)

export const stringQ: Doc = new Doc(
  '(string? v): boolean?', [
    'v: any',
  ],
  'Returns `#t` if and only `v` is a string.'
)

export const makeString: Doc = new Doc(
  '(make-string k c): string?', [
    'k: integer?',
    'c: char?',
  ],
  'Returns a string of length `k` with each character set to `c`.'
)

export const string: Doc = new Doc(
  '(string c1 c2 ...): string?', [
    'c: char?',
  ],
  'Returns a string consisting of the characters `c1`, `c2`, ...'
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

export const stringEq: Doc = new Doc(
  '(string=? s1 s2 ...): boolean?', [
    's: string?',
  ],
  'Returns `#t` if and only `s1`, `s2`, ... are equivalent strings.'
)

export const stringLt: Doc = new Doc(
  '(string<? s1 s2 ...): boolean?', [
    's: string?',
  ],
  'Returns `#t` if and only `s1`, `s2`, ... are in strictly lexicographically increasing order.'
)

export const stringGt: Doc = new Doc(
  '(string>? s1 s2 ...): boolean?', [
    's: string?',
  ],
  'Returns `#t` if and only `s1`, `s2`, ... are in strictly lexicographically decreasing order.'
)

export const stringLeq: Doc = new Doc(
  '(string<=? s1 s2 ...): boolean?', [
    's: string?',
  ],
  'Returns `#t` if and only `s1`, `s2`, ... are in lexicographical order.'
)

export const stringGeq: Doc = new Doc(
  '(string>=? s1 s2 ...): boolean?', [
    's: string?',
  ],
  'Returns `#t` if and only `s1`, `s2`, ... are in reverse lexicographical order.'
)

export const stringEqCi: Doc = new Doc(
  '(string-ci=? s1 s2 ...): boolean?', [
    's: string?',
  ],
  'Returns `#t` if and only `s1`, `s2`, ... are equivalent strings, ignoring case.'
)

export const stringLtCi: Doc = new Doc(
  '(string-ci<? s1 s2 ...): boolean?', [
    's: string?',
  ],
  'Returns `#t` if and only `s1`, `s2`, ... are in strictly lexicographically increasing order, ignoring case.'
)

export const stringGtCi: Doc = new Doc(
  '(string-ci>? s1 s2 ...): boolean?', [
    's: string?',
  ],
  'Returns `#t` if and only `s1`, `s2`, ... are in strictly lexicographically decreasing order, ignoring case.'
)

export const stringLeqCi: Doc = new Doc(
  '(string-ci<=? s1 s2 ...): boolean?', [
    's: string?',
  ],
  'Returns `#t` if and only `s1`, `s2`, ... are in lexicographical order, ignoring case.'
)

export const stringGeqCi: Doc = new Doc(
  '(string-ci>=? s1 s2 ...): boolean?', [
    's: string?',
  ],
  'Returns `#t` if and only `s1`, `s2`, ... are in reverse lexicographical order, ignoring case.'
)

export const stringUpcase: Doc = new Doc(
  '(string-upcase s): string?', [
    's: string?',
  ],
  'Returns the upper-case version of `s`.'
)

export const stringDowncase: Doc = new Doc(
  '(string-downcase s): string?', [
    's: string?',
  ],
  'Returns the lower-case version of `s`.'
)

export const stringFoldcase: Doc = new Doc(
  '(string-foldcase s): string?', [
    's: string?',
  ],
  'Returns the case-folded version of `s`. This is a version of `s` that is appropriate for case-insensitive comparison.'
)

export const substring: Doc = new Doc(
  '(substring s start end): string?', [
    's: string?',
    'start: integer?',
    'end: integer?',
  ],
  'Returns the substring of `s` from index `start` (inclusive) to index `end` (exclusive).'
)

export const stringAppend: Doc = new Doc(
  '(string-append s1 s2 ...): string?', [
    's: string?',
  ],
  'Returns a string made by joining `s1`, `s2`, ... together.'
)

export const stringList: Doc = new Doc(
  '(string->list s): list?', [
    's: string?',
  ],
  'Returns a list of the characters in `s`.'
)

export const listString: Doc = new Doc(
  '(list->string l): string?', [
    'l: list?',
  ],
  'Returns a string made by joining the characters in `l` together.'
)

export const stringSplit: Doc = new Doc(
  '(string-split s sep): list?', [
    's: string?',
    'sep: string?',
  ],
  'Returns a list of strings obtained by splitting `s` at occurrences of `sep`.'
)

export const procedure: Doc = new Doc(
  '(procedure? v): boolean?', [
    'v: any',
  ],
  'Returns `#t` if and only `v` is a procedure.'
)

export const apply: Doc = new Doc(
  '(apply f l): any', [
    'f: procedure?',
    'l: list?',
  ],
  'Calls `f` with the values contained in `l`.'
)

export const stringMap: Doc = new Doc(
  '(string-map f s): string?', [
    'f: procedure?, a function from characters to characters',
    's: string?',
  ],
  'Returns a new string containing the results of applying `f` to each character of `s`.'
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

export const error: Doc = new Doc(
  '(error msg): any', [
    'msg: string?',
  ],
  'Raises a runtime error with message `msg`.'
)

export const qq: Doc = new Doc(
  '{??}: any', [],
  'A placeholder for an expression that is not yet implemented.'
)

export const compose: Doc = new Doc(
  '(compose f1 f2 ...): procedure?', [
    'f: procedure?',
  ],
  'Returns a new procedure that is the composition of the given functions, _i.e._, `f(x) = f1(f2(...(fk(x))))`.'
)

export const o: Doc = new Doc(
  `(o f1 f2 ...): procedure?`, [
    'f: procedure?',
  ],
  'A synonym for `compose`.'
)

export const pipe: Doc = new Doc(
  `(|> v f1 f2 ...): any`, [
    'v: any',
    'f: procedure?',
  ],
  'Returns the result of applying the given function in sequence, starting with initial value `v`, _i.e._, `fk(fk-1(...(f1(v))))`.'
)

export const range: Doc = new Doc(
  '(range beg end): list?', [
    'beg: integer?, this argument can be omitted',
    'end: integer?, n >= 0',
  ],
  'Returns a list containing the numbers from `beg` to `end` (exclusive). If `beg` is not given, it defaults to 0.'
)

export const elseV: Doc = new Doc(
  'else: ?boolean', [], 
  'A synonym for `#t` appropriate for use as the final guard of a `cond` expression.'
)

///// image ////////////////////////////////////////////////////////////////////

export const image: Doc = new Doc(
  '(image? v): boolean?', [
    'v: any',
  ],
  'Returns `#t` if and only `v` is an image.'
)

export const color: Doc = new Doc(
  '(color r b g a): string?', [
    'r: integer?, 0 <= r <= 255',
    'b: integer?, 0 <= b <= 255',
    'g: integer?, 0 <= g <= 255',
    'a: integer?, 0 <= a <= 1'
  ],
  'Returns a string of the form `"rgba(r, g, b, a)"` appropriate for use as a color.'
)

export const ellipse: Doc = new Doc(
  '(ellipse width height fill color): drawing?', [
    'width: integer?',
    'height: integer?',
    'fill: boolean?',
    'color: string?',
  ],
  'Returns a new drawing containing an ellipse with dimensions `width × height`.'
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
  'Returns a drawing consisting of a rectangle with dimensions `width × height`.'
)

export const drawingSquare: Doc = new Doc(
  '(square width fill color): drawing?', [
    'width: number?',
    'fill: string?, either "solid" or "outline"',
    'color: string?, either a color name or the form "rgba(r, g, b, a)"'
  ],
  'Returns a drawing consisting of a square with length `width`.'
)

export const triangle: Doc = new Doc(
  '(triangle length fill color): drawing?', [
    'length: number?',
    'fill: string?, either "solid" or "outline"',
    'color: string?, either a color name or the form "rgba(r, g, b, a)"'
  ],
  'Returns a drawing consisting of a equilateral triangle with length `length`.'
)

export const path: Doc = new Doc(
  '(path width height points fill color): drawing?', [
    'width: number?',
    'height: number?',
    'points: list?, a list of points, pairs of numbers',
    'fill: string?, either "solid" or "outline"',
    'color: string?, either a color name or the form "rgba(r, g, b, a)"'
  ],
  'Returns a drawing with dimensions `width × height` formed by connecting the points in `points` with straight lines. The points are specified as a `pair` of coordinates.'
)

export const beside: Doc = new Doc(
  '(beside d1 d2 ...): drawing?', [
    'd: drawing?'
  ],
  'Creates a new drawing formed by places the drawing `d1`, `d2`, ..., beside each other (horizontally).'
)

export const besideAlign: Doc = new Doc(
  '(beside/align align d1 d2 ...): drawing?', [
    'align: string?, either "top", "center", or "bottom"',
    'd: drawing?'
  ],
  'Creates a new drawing formed by places the drawing `d1`, `d2`, ..., beside each other on the x-axis, aligning them along the y-axis according to `align`.'
)

export const above: Doc = new Doc(
  '(above d1 d2 ...): drawing?', [
    'd: drawing?'
  ],
  'Creates a new drawing formed by places the drawing `d1`, `d2`, ..., above each other (vertically in descending order).'
)

export const aboveAlign: Doc = new Doc(
  '(above/align align d1 d2 ...): drawing?', [
    'align: string?, either "left", "middle", or "right"',
    'd: drawing?'
  ],
  'Creates a new drawing formed by places the drawing `d1`, `d2`, ..., above each other on the y-axis, aligning them along the x-axis according to `align`.'
)

export const overlay: Doc = new Doc(
  '(overlay d1 d2 ...): drawing?', [
    'd: drawing?'
  ],
  'Creates a new drawing formed by places the drawing `d1`, `d2`, ..., on top of each other. (`d1` is the topmost drawing).'
)

export const overlayAlign: Doc = new Doc(
  '(overlay/align xAlign yAlign d1 d2 ...): drawing?', [
    'xAlign: string?, either "left", "middle", or "right"',
    'yAlign: string?, either "top", "center", or "bottom"',
    'd: drawing?'
  ],
  'Creates a new drawing formed by places the drawing `d1`, `d2`, ..., on top of each other, aligning them according to `xAlign` and `yAlign`.'
)

export const overlayOffset: Doc = new Doc(
  '(overlay/offset d1 dx dy d2): drawing?', [
    'd1: drawing?',
    'dx: number?',
    'dy: number?',
    'd2: drawing?',
  ],
  'Creates a new drawing formed by places the drawing `d1` on top of `d2`, offset by `(dx, dy)`.'
)

export const rotate: Doc = new Doc(
  '(rotate angle d): drawing?', [
    'angle: number?, in degrees',
    'd: drawing?',
  ],
  'Returns a new drawing formed by rotating drawing `d` by `angle` degrees around the center of its bounding box. Note: currently buggy and shifts off-center.'
)

export const withDashes: Doc = new Doc(
  '(with-dashes dash-spec d): drawing?', [
    'dash-spec: list?, a list of numbers',
    'd: drawing?',
  ],
  'Returns a new drawing formed by drawing `d` but with dashes specified by `dash-spec`. `dash-spec` is an list of numbers where each successive pair of numbers describe the length of a dash and the length of the subsequent gap.'
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

export const numerator: Doc = new Doc(
  '(numerator dur): integer?', [
    'dur: duration?'
  ],
  'Returns the numerator of `dur`.'
)

export const denominator: Doc = new Doc(
  '(denominator dur): integer?', [
    'dur: duration?'
  ],
  'Returns the denominator of `dur`.'
)

export const empty: Doc = new Doc(
  'empty: composition?', [], 'The empty composition.'
)

export const note: Doc = new Doc(
  '(note midi-note dur): composition?', [
    'midi-note: integer?, 0 <= midi-note <= 127',
    'dur: dur?'
  ],
  'Creates a new composition consisting of a single note from the given MIDI note value and duration.'
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

export const pickup: Doc = new Doc(
  '(pickup c1 c2): composition?', [
    'c1: composition?',
    'c2: composition?',
  ],
  'Creates a new composition that plays `c2` preceded by `c1`. `c1`\'s duration is not factored into the duration of the overall composition.'
)

export const mod: Doc = new Doc(
  '(mod kind comp): composition?', [
    'kind: mod?',
    'comp: composition?',
  ],
  'Creates a new composition that plays `comp` with the given modification `mod`.'
)

export const band: Doc = new Doc(
  '(band inst1 ... inst8): composition?', [
    'inst: number?, a valid MIDI instrument program number (1--128)',
  ],
  'Creates a new composition that plays `comp` with the given instruments, where the _i_th instrument is assigned to the _i_th MIDI channel. Individual channels can be selected for playback using the `instrument` mod.'
)

export const instrument: Doc = new Doc(
  '(instrument ch): composition?', [
    'ch: integer?, a valid MIDI channel number (0--9)',
  ],
  'Creates a new composition that plays composition `comp` played through MIDI channel `ch`. The instruments voiced in each channel ber set with the `band` mod. Channel 9 is reserved for percussion sounds.'
)

export const percussion: Doc = new Doc(
  'percussion: mod?', [], 'A modification that switches playback to percussion mode (MIDI channel 9). In percussion mode, each note corresponds to one percussion instrument.'
)

export const bend: Doc = new Doc(
  '(bend amount): composition?', [
    'amount: number?, -1 <= amount <= 1',
  ],
  'Creates a new composition where the pitch is bent by a factor of `amount × 2` semitones. If `amount = 0`, then the pitch is played normally.'
)

export const tempo: Doc = new Doc(
  '(tempo beat bpm comp): composition?', [
    'beat: dur?, the pulse of the tempo',
    'bpm: number?, beats per minute',
    'comp: composition?',
  ],
  'Creates a new composition that plays `comp` at the given `beat` and `bpm`.'
)

export const dynamics: Doc = new Doc(
  '(dynamics velocity comp): composition?', [
    'velocity: integer?, 0 <= level <= 127',
    'comp: composition?',
  ],
  'Creates a new composition that plays `comp` at the given MIDI `velocity` value. Note that a `velocity` of `127` corresponds to full volume for that note.'
)

export const repeat: Doc = new Doc(
  '(repeat n comp): composition?', [
    'n: integer?, n >= 0',
    'comp: composition?',
  ],
  'Creates a new composition formed by repeating `comp` `n` times sequentially.'
)

export const wn: Doc = new Doc(
  'wn: dur?', [], 'A whole note duration (4/4).'
)

export const hn: Doc = new Doc(
  'hn: dur?', [], 'A half note duration (2/4).'
)

export const qn: Doc = new Doc(
  'qn: dur?', [], 'A quarter note duration (1/4).'
)

export const en: Doc = new Doc(
  'en: dur?', [], 'An eighth note duration (1/8).'
)

export const sn: Doc = new Doc(
  'sn: dur?', [], 'A sixteenth note duration (1/16).'
)

export const tn: Doc = new Doc(
  'tn: dur?', [], 'A thirty-secondth note duration (1/32).'
)
