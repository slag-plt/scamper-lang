import { asBool_, asNum_, nlebool, nlenumber, EPair, Exp, expEquals, isBoolean, isList, isNumber, isPair, isReal, isInteger, epair, nlestr, nlenil, nlepair, expToString, isString, asString_, nlechar, nlecall, Prim, isProcedure, arrayToList, unsafeListToArray, Env, entry, nleprim } from '../lang.js'
import { ICE, ok, Result } from '../result.js'
import { evaluateExp, runtimeError } from '../runtime.js'
import { msg } from '../messages.js'
import { Doc } from './docs.js'

function asNumbers (args: Exp[]): Result<number[]> {
  const result = new Array(args.length)
  for (let i = 0; i < args.length; i++) {
    const e = args[i]
    if (e.tag === 'lit') {
      if (e.value.tag === 'num') {
        result[i] = e.value.value
      } else {
        return runtimeError(msg('error-type-expected', 'number', e.value.tag), e)
      }
    } else {
      return runtimeError(msg('error-type-expected', 'number', e.tag))
    }
  }
  return ok(result)
}

const preludeEntry = (prim: Prim, docs?: string) => entry(nleprim(prim), 'prelude', undefined, docs)

// Equivalence predicates (6.1)

// TODO: implement:
//   (eqv? x y)
//   (eq? x y)
// ... do I? Do we need these different equivalence notions?

const equalDoc: Doc = new Doc(
  '(equal? v1 v2): boolean', [
    'v1: any',
    'v2: any',
  ],
  'Returns `#t` if and only `v1` and `v2` are (structurally) equal values.'
)

const equalPrim: Prim = (_env, args, app) =>
  args.length === 2
    ? ok(nlebool(expEquals(args[0], args[1])))
    : runtimeError(msg('error-arity', 'equal?', '2', args.length), app)

const equivalencePrimitives: [string, Prim, Doc | undefined][] = [
  ['equal?', equalPrim, equalDoc]
]

// Numbers (6.2)

const numberDoc: Doc = new Doc(
  '(number? v): boolean', [
    'v: any'
  ],
  'Returns `#t` if and only `v` is a number.'
)

const numberPrim: Prim = (_env, args, app) =>
  args.length === 1
    ? ok(nlebool(isNumber(args[0])))
    : runtimeError(msg('error-arity', 'number?', '1', args.length), app)

const realDoc: Doc = new Doc(
  '(real? v): boolean', [
    'v: any'
  ],
  'Returns `#t` if and only `v` is a real number.'
)

const realPrim: Prim = (_env, args, app) =>
  args.length === 1
    ? ok(nlebool(isReal(args[0])))
    : runtimeError(msg('error-arity', 'real?', '1', args.length), app)

const integerDoc: Doc = new Doc(
  '(integer? v): boolean', [
    'v: any'
  ],
  'Returns `#t` if and only `v` is an integer.'
)

const integerPrim: Prim = (_env, args, app) =>
  args.length === 1
    ? ok(nlebool(isInteger(args[0])))
    : runtimeError(msg('error-arity', 'integer?', '1', args.length), app)

// TODO: implement:
//   (complex? obj)
//   (rational? obj)
//
// ...probably only implement the subset of these that make sense for the Javascript numeric stack:
//   number -> real -> integer

// TODO: implement:
//   (exact? z)
//   (inexact? z)
//   (exact->integer? z)
//   (finite? z)
//   (infinite? z)
//   (nan? z)

function compareOp (symbol: string, op: (x: number, y: number) => boolean, args: Exp[], app: Exp): Result<Exp> {
  return args.length === 2
    ? asNumbers(args).andThen(vs => ok(nlebool(op(vs[0], vs[1]))))
    : runtimeError(msg('error-arity', `(${symbol})`, '2', args.length), app)
}

const ltPrim: Prim = (_env, args, app) => compareOp('<', (x, y) => x < y, args, app)
const leqPrim : Prim = (_env, args, app) => compareOp('<=', (x, y) => x <= y, args, app)
const gtPrim : Prim = (_env, args, app) => compareOp('>', (x, y) => x > y, args, app)
const geqPrim : Prim = (_env, args, app) => compareOp('>=', (x, y) => x >= y, args, app)
const numeqPrim : Prim = (_env, args, app) => compareOp('=', (x, y) => x === y, args, app)

const zeroPrim : Prim = (_env, args, app) =>
  args.length !== 1
    ? runtimeError(msg('error-arity', 'zero?', '1', args.length), app)
    : !isNumber(args[0])
        ? runtimeError(msg('error-type-expected-fun', 'zero?', 'number', args[0].tag), app)
        : ok(nlebool(asNum_(args[0]) === 0))

const positivePrim : Prim = (_env, args, app) =>
  args.length !== 1
    ? runtimeError(msg('error-arity', 'positive?', '1', args.length), app)
    : !isNumber(args[0])
        ? runtimeError(msg('error-type-expected-fun', 'positive?', 'number', args[0].tag), app)
        : ok(nlebool(asNum_(args[0]) > 0))

const negativePrim : Prim = (_env, args, app) =>
  args.length !== 1
    ? runtimeError(msg('error-arity', 'negative?', '1', args.length), app)
    : !isNumber(args[0])
        ? runtimeError(msg('error-type-expected-fun', 'negative?', 'number', args[0].tag), app)
        : ok(nlebool(asNum_(args[0]) < 0))

const oddPrim : Prim = (_env, args, app) =>
  args.length !== 1
    ? runtimeError(msg('error-arity', 'odd?', '1', args.length), app)
    : !isNumber(args[0])
        ? runtimeError(msg('error-type-expected-fun', 'odd?', 'number', args[0].tag), app)
        : ok(nlebool((asNum_(args[0]) & 1) === 1))

const evenPrim : Prim = (_env, args, app) =>
  args.length !== 1
    ? runtimeError(msg('error-arity', 'even?', '1', args.length), app)
    : !isNumber(args[0])
        ? runtimeError(msg('error-type-expected-fun', 'even?', 'number', args[0].tag), app)
        : ok(nlebool((asNum_(args[0]) & 1) !== 1))

function numericUOp (symbol: string, op: (x: number) => number, args: Exp[], app: Exp): Result<Exp> {
  return args.length !== 1
    ? runtimeError(msg('error-arity', `(${symbol})`, '1', args.length), app)
    : asNumbers(args).andThen(vs => ok(nlenumber(op(vs[0]))))
}

function numericBOp (symbol: string, op: (x: number, y: number) => number, args: Exp[], app: Exp): Result<Exp> {
  return args.length !== 2
    ? runtimeError(msg('error-arity', `(${symbol})`, '2', args.length), app)
    : asNumbers(args).andThen(vs => ok(nlenumber(op(vs[0], vs[1]))))
}

function numericNOp (symbol: string, op: (x: number, y: number) => number, args: Exp[], app: Exp): Result<Exp> {
  return args.length === 0
    ? runtimeError(msg('error-arity-atleast', `(${symbol})`, '1', args.length), app)
    : asNumbers(args).andThen(vs => ok(nlenumber(vs.reduce((result, v) => op(result, v)))))
}
const maxPrim: Prim = (_env, args, app) => numericNOp('max', Math.max, args, app)
const minPrim: Prim = (_env, args, app) => numericNOp('min', Math.min, args, app)

const plusPrim: Prim = (_env, args, app) => numericNOp('+', (x, y) => x + y, args, app)
const minusPrim: Prim = (_env, args, app) => numericNOp('-', (x, y) => x - y, args, app)
const timesPrim: Prim = (_env, args, app) => numericNOp('*', (x, y) => x * y, args, app)
const divPrim: Prim = (_env, args, app) => numericNOp('/', (x, y) => x / y, args, app)

const absPrim: Prim = (_env, args, app) => numericUOp('abs', (x) => Math.abs(x), args, app)

// TODO: implement:
//   (floor / n1 n2)
//   (floor-quotient n1 n2)
//   (floor-remainder n1 n2)
//   (truncate/ n1 n2)
//   (truncate-quotient n1 n2)
//   (truncate-remainder n1 n2)
//   (quotient n1 n2)
//   (remainder n1 n2)

const moduloPrim: Prim = (_env, args, app) => numericBOp('modulo', (x, y) => x % y, args, app)

//   (modulo n1 n2)

// TODO: implement:
//   (gcd n1 ...)
//   (lcm n1 ...)
//   (numerator q)    ...wait, do we need these?
//   (denominator q)  ...wait, do we need these?

const floorPrim: Prim = (_env, args, app) => numericUOp('floor', (x) => Math.floor(x), args, app)
const ceilingPrim: Prim = (_env, args, app) => numericUOp('ceiling', (x) => Math.ceil(x), args, app)
const truncatePrim: Prim = (_env, args, app) => numericUOp('truncate', (x) => Math.trunc(x), args, app)
const roundPrim: Prim = (_env, args, app) => numericUOp('round', (x) => Math.round(x), args, app)

// TODO: implement:
//   (rationalize x y)

const squarePrim: Prim = (_env, args, app) => numericUOp('square', (x) => Math.pow(x, 2), args, app)
const sqrtPrim: Prim = (_env, args, app) => numericUOp('sqrt', (x) => Math.sqrt(x), args, app)

// TODO: implement:
//   (exact-integer-sqrt k)

const exptPrim: Prim = (_env, args, app) => numericBOp('expt', (x, y) => Math.pow(x, y), args, app)

// TODO: implement:
//   (make-rectangular x1 x2)   ...probably not!
//   (make-polar x3 x4)         ...probably not!
//   (real-part z)              ...probably not!
//   (imag-part z)              ...probably not!
//   (magnitude z)              ...probably not!
//   (angle z)                  ...probably not!

const numberStringPrim: Prim = (_env, args, app) => {
  // TODO: support (number->string z radix)?
  if (args.length !== 1) { return runtimeError(msg('error-arity', 'number->string', '1', args.length), app) }
  const e = args[0]
  if (e.tag === 'lit' && e.value.tag === 'num') {
    return ok(nlestr(e.value.value.toString()))
  } else {
    return runtimeError(msg('error-type-expected-fun', 'number->string', 'number', e.tag), app)
  }
}

// TODO: implement:
//   (string->number s)
//   (string->number s radix)

const numericPrimitives: [string, Prim, Doc | undefined][] = [
  ['number?', numberPrim, numberDoc],
  ['real?', realPrim, realDoc],
  ['integer?', integerPrim, integerDoc],
  ['<', ltPrim, undefined],
  ['<=', leqPrim, undefined],
  ['>', gtPrim, undefined],
  ['>=', geqPrim, undefined],
  ['=', numeqPrim, undefined],
  ['zero?', zeroPrim, undefined],
  ['positive?', positivePrim, undefined],
  ['negative?', negativePrim, undefined],
  ['odd?', oddPrim, undefined],
  ['even?', evenPrim, undefined],
  ['max', maxPrim, undefined],
  ['min', minPrim, undefined],
  ['+', plusPrim, undefined],
  ['-', minusPrim, undefined],
  ['*', timesPrim, undefined],
  ['/', divPrim, undefined],
  ['abs', absPrim, undefined],
  ['modulo', moduloPrim, undefined],
  ['floor', floorPrim, undefined],
  ['ceiling', ceilingPrim, undefined],
  ['truncate', truncatePrim, undefined],
  ['round', roundPrim, undefined],
  ['square', squarePrim, undefined],
  ['sqrt', sqrtPrim, undefined],
  ['expt', exptPrim, undefined],
  ['number->string', numberStringPrim, undefined]
]

// Booleans (6.3)

const notPrim: Prim = (_env, args, app) =>
  args.length !== 1
    ? runtimeError(msg('error-arity', 'not', '1', args.length), app)
    : isBoolean(args[0])
      ? runtimeError(msg('error-type-expected-fun', 'not', 'boolean', args[0].tag), app)
      : ok(nlebool(!asBool_(args[0])))

const booleanPrim: Prim = (_env, args, app) =>
  args.length === 1
    ? ok(nlebool(isBoolean(args[0])))
    : runtimeError(msg('error-arity', 'boolean?', '1', args.length), app)

const booleanPrimitives: [string, Prim, Doc | undefined][] = [
  ['not', notPrim, undefined],
  ['boolean?', booleanPrim, undefined]
]

// Pairs and Lists (6.4)

const pairPrim: Prim = (_env, args, app) =>
  args.length === 1
    ? ok(nlebool(isPair(args[0])))
    : runtimeError(msg('error-arity', 'pair?', '1', args.length), app)

const consPrim: Prim = (_env, args, app) =>
  args.length === 2
    ? ok(epair(app.range, args[0], args[1]))
    : runtimeError(msg('error-arity', 'cons', '2', args.length), app)

const carPrim : Prim = (_env, args, app) =>
  args.length !== 1
    ? runtimeError(msg('error-arity', 'car', '1', args.length), app)
    : !isPair(args[0])
        ? runtimeError(msg('error-type-expected-fun', 'car', 'pair', args[0].tag), app)
        : ok((args[0] as EPair).e1)

const cdrPrim : Prim = (_env, args, app) =>
  args.length !== 1
    ? runtimeError(msg('error-arity', 'cdr', '1', args.length), app)
    : !isPair(args[0])
        ? runtimeError(msg('error-type-expected-fun', 'cdr', 'pair', args[0].tag), app)
        : ok((args[0] as EPair).e2)

// N.B., set-car! and set-cdr! are unimplemented since we only implement the
// pure, functional subset of Scheme.

// TODO: implement caar, cadr, cdar, cddr, caaar, ..., cdddr in some elegant way

const nullPrim : Prim = (_env, args, app) =>
  args.length === 1
    ? ok(nlebool(args[0].tag === 'nil'))
    : runtimeError(msg('error-arity', 'null?', '1', args.length), app)

const listQPrim : Prim = (_env, args, app) =>
  args.length === 1
    ? ok(nlebool(isList(args[0])))
    : runtimeError(msg('error-arity', 'list?', '1', args.length), app)

const pairListPrimitives: [string, Prim, Doc | undefined][] = [
  ['pair?', pairPrim, undefined],
  ['cons', consPrim, undefined],
  ['car', carPrim, undefined],
  ['cdr', cdrPrim, undefined],
  ['null?', nullPrim, undefined],
  ['list?', listQPrim, undefined]
]

const listPrim: Prim = function (_env, args, app) {
  return ok(arrayToList(args))
}

const makeListPrim: Prim = function (_env, args, app) {
  // N.B., (make-list k) returns the empty list, but this behavior is weird, so we don't replicate it!
  if (args.length !== 2) {
    return runtimeError(msg('error-arity', 'make-list', '2', args.length), app)
  }
  if (!isNumber(args[0])) {
    return runtimeError(msg('error-type-expected-fun', 'make-list', 'integer', args[0].tag), app)
  }
  const n = asNum_(args[0])
  const fill = args[1]
  let ret: Exp = nlenil()
  for (let i = 0; i < n; i++) {
    ret = nlepair(fill, ret)
  }
  return ok(ret)
}

const lengthPrim: Prim = function (_env, args, app) {
  if (args.length !== 1) {
    return runtimeError(msg('error-arity', 'length', '1', args.length), app)
  }
  const length = 0
  let e: Exp = args[0]
  while (e.tag !== 'nil') {
    if (e.tag === 'pair') {
      e = e.e2
    } else {
      return runtimeError(msg('error-type-expected-fun', 'length', 'list', e.tag), app)
    }
  }
  return ok(nlenumber(length))
}

function appendOne_ (l1: Exp, l2: Exp): Exp {
  if (l1.tag === 'nil') {
    return l2
  } else if (l1.tag === 'pair') {
    return nlepair(l1.e1, appendOne_(l1.e2, l2))
  } else {
    throw new ICE('appendOne', `Non-list passed: ${expToString(l1)}`)
  }
}

const appendPrim: Prim = function (_env, args, app) {
  if (args.length === 0) {
    return runtimeError(msg('error-arity-atleast', 'append', '1', args.length))
  }
  args.forEach(e => {
    if (!isList(e)) {
      return runtimeError(msg('error-type-expected-fun', 'append', 'list', e.tag), e)
    }
  })
  let ret = args[0]
  for (let i = 1; i < args.length; i++) {
    ret = appendOne_(ret, args[i])
  }
  return ok(ret)
}

// TODO: implement:
//   (reverse list)
//   (list-tail list k)
//   (list-ref list k)

// N.B., list-set! is unimplemented since it is effectful.

// TODO: implement:
//   (memq obj list)
//   (memv obj list)
//   (member obj list)
//   (member obj list compare)
//   (assq obj alist)
//   (assv obj alist)
//   (assoc obj alist)
//   (assoc obj alist compare)
//   (list-copy obj)

const listPrimitives: [string, Prim, Doc | undefined][] = [
  ['list', listPrim, undefined],
  ['make-list', makeListPrim, undefined],
  ['length', lengthPrim, undefined],
  ['append', appendPrim, undefined]
]

// Symbols (6.5)

// TODO: implement:
//   (symbol? obj)
//   (symbol=? sym1 ... symk)
//   (symbol->string sym)
//   (string->symbol str)
//
// ...but we don't implement symbols, will we?

// Characters (6.6)

// TODO: implement:
//   (char? obj)
//   (char=? char1 ... chark)
//   (char<? char1 ... chark)
//   (char>? char1 ... chark)
//   (char<=? char1 ... chark)
//   (char>=? char1 ... chark)
//   (char-ci=? char1 ... chark)
//   (char-ci<? char1 ... chark)
//   (char-ci>? char1 ... chark)
//   (char-ci<=? char1 ... chark)
//   (char-ci>=? char1 ... chark)
//   (char-alphabetic? char)
//   (char-numeric? char)
//   (char-whitespace? letter)
//   (char-upper-case? letter)
//   (char-lower-case? letter)
//   (digit-value char)
//   (char->integer char)
//   (integer->char n)
//   (char-upcase char)
//   (char-downcase char)
//   (char-foldcase char)
//
// ...but there are no characters in Javascript. How should we implement them?

// Strings (6.7)

const stringPrim: Prim = (_env, args, app) =>
  args.length !== 1
    ? runtimeError(msg('error-arity', 'string?', '1', args.length), app)
    : ok(nlebool(isString(args[0])))

// TODO: implement:
//   (make-string k)
//   (make-string k char)
//   (string char ...)

const stringLengthPrim: Prim = (_env, args, app) =>
  args.length !== 1
    ? runtimeError(msg('error-arity', 'string-length', '1', args.length), app)
    : isString(args[0])
      ? ok(nlestr(asString_(args[0])))
      : runtimeError(msg('error-type-expected-fun', 'string-length', 'string', args[0].tag), app)

const stringRefPrim: Prim = function (_env, args, app) {
  if (args.length !== 2) {
    return runtimeError(msg('error-arity', 'string-ref', '2', args.length), app)
  } else if (!isString(args[0])) {
    return runtimeError(msg('error-type-expected-fun', 'string-ref', 'string', args[0].tag), args[0])
  } else if (!isInteger(args[1])) {
    return runtimeError(msg('error-type-expected-fun', 'string-ref', 'integer', args[1].tag), args[1])
  }
  const str = asString_(args[0])
  const i = asNum_(args[1])
  if (i >= 0 && i < str.length) {
    return ok(nlechar(str[i]))
  } else {
    return runtimeError(msg('error-index-string', i, str), app)
  }
}

// N.B., string-set! is unimplemented since it is effectful.

// TODO: implement:
//   (string=? str1 ... strk)
//   (string<? str1 ... strk)
//   (string>? str1 ... strk)
//   (string<=? str1 ... strk)
//   (string>=? str1 ... strk)
//   ...and their string-ci equivalents...
//   (string-upcase str)
//   (string-downcase str)
//   (string-foldcase str)
//   (substring str start end)
//   (string-append str1 ... strk)
//   (string->list string)
//   (string->list string start end)
//   (string->list string start end)
//   (list->string list)
//   (string-copy string)
//   (string-copy string start)
//   (string-copy string start end)
//
// ...or some subset of these, at least.

// N.B., string-copy! and string-fill! are unimplemented since they are effectful.

const stringPrimitives: [string, Prim, Doc | undefined][] = [
  ['string?', stringPrim, undefined],
  ['string-length', stringLengthPrim, undefined],
  ['string-ref', stringRefPrim, undefined]
]

// Vectors (6.8)

// N.B., vector operations are unimplemented because they are inherently effectful.

// Bytevectors (6.9)

// N.B., bytevector operations are unimplemented because they are inherently effectful.

// Control features (6.10)

const procedurePrim: Prim = (_env, args, app) =>
  // TODO: procedure? should also return true for variables that are bound
  // to procedures in the environment. Probably means that we need to thread
  // the environment through primitives, ick!
  args.length !== 1
    ? runtimeError(msg('error-arity', 'procedure?', '1', args.length), app)
    : ok(nlebool(isProcedure(args[0])))

const applyPrim: Prim = (env, args, app) =>
  args.length === 0
    ? runtimeError(msg('error-arity-atleast', 'apply', '1', args.length), app)
    : evaluateExp(env, nlecall(args[0], [...args.slice(1)]))

// TODO: for map, do we expand to [f(x1), f(x2), ...] or do we step through
// the full transformation? Probably step through the full transformation, but
// then we also need to be able to step, creating a circularity in our
// dependencies, eek!

// TODO: implement:
//   (string-map fn str1 ... strk)

const mapPrim: Prim = (env, args, app) =>
  args.length !== 2
    ? runtimeError(msg('error-arity', 'map', '2', args.length), app)
    : !isProcedure(args[0])
        ? runtimeError(msg('error-type-expected-fun', 'map', 'procedure', args[0].tag), app)
        : !isList(args[1])
            ? runtimeError(msg('error-type-expected-fun', 'map', 'list', args[1].tag), app)
            : evaluateExp(env, arrayToList(unsafeListToArray(args[1]).map(e => nlecall(args[0], [e]))))

// N.B., (vector-map fn v1 ... vk) not implemented since vectors are not implemented.

// TODO: implement:
//   (for-each fn l1 ... lk)
//   (string-for-each fn str1 ... strk)

// N.B., (vector-for-each fn v1 ... vk) not implemented since vectors are not implemented.

// TODO: implement:
//   (call-with-current-continuation proc)
//   (call/cc proc)
//   (values obj ...)
//   (call-with-values producer consumer)
//   (dynamic-wind before thunk after)

const controlPrimitives: [string, Prim, Doc | undefined][] = [
  ['procedure?', procedurePrim, undefined],
  ['apply', applyPrim, undefined],
  ['map', mapPrim, undefined]
]

// Exceptions (6.11)

// N.B., exception operations are unimplemented because they are inherently effectful.

// Environments and Evaluation (6.12)

// N.B., platform-specific stuff with no need to be implemented.

// Input andoutput (6.13)

// N.B., in-browser, so can't implement directly without some level of virtualization.

// System interface (6.14)

// N.B., not implemented, all operating system-specific stuff.

export const preludeEnv = new Env([
  ...equivalencePrimitives,
  ...numericPrimitives,
  ...booleanPrimitives,
  ...pairListPrimitives,
  ...listPrimitives,
  ...stringPrimitives,
  ...controlPrimitives
].map(v => [v[0], entry(nleprim(v[1]), 'prelude', undefined, v[2] ? v[2].docToMarkdown() : '')]))