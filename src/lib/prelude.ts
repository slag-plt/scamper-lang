import { Doc, asBool_, asNum_, nlebool, nlenumber, EPair, Exp, expEquals, isBoolean, isList, isNumber, isPair, isReal, isInteger, epair, nlestr, nlenil, nlepair, expToString, isString, asString_, nlechar, nlecall, Prim, isProcedure, arrayToList, unsafeListToArray, Env, entry, nleprim } from '../lang.js'
import { ICE, ok, Result, rethrow } from '../result.js'
import { evaluateExp, runtimeError } from '../runtime.js'
import * as Utils from './utils.js'
import { msg } from '../messages.js'
import * as Docs from './docs.js'

function asNumbers (args: Exp[]): Result<number[]> {
  const result = new Array(args.length)
  for (let i = 0; i < args.length; i++) {
    const e = args[i]
    if (e.tag === 'lit') {
      if (e.value.tag === 'num') {
        result[i] = e.value.value
      } else {
        return runtimeError(msg('error-type-expected', 'number?', e.value.tag), e)
      }
    } else {
      return runtimeError(msg('error-type-expected', 'number?', e.tag))
    }
  }
  return ok(result)
}

const preludeEntry = (prim: Prim, docs?: Doc) => entry(nleprim(prim), 'prelude', undefined, docs)

// Equivalence predicates (6.1)

// TODO: implement:
//   (eqv? x y)
//   (eq? x y)
// ... do I? Do we need these different equivalence notions?

const equalPrim: Prim = (_env, args, app) =>
  Utils.checkArgsResult('equal?', ['any', 'any'], undefined, args, app).andThen(_ =>
    ok(nlebool(expEquals(args[0], args[1]))))

const equivalencePrimitives: [string, Prim, Doc | undefined][] = [
  ['equal?', equalPrim, Docs.equal]
]

// Numbers (6.2)

const numberPrim: Prim = (_env, args, app) =>
  Utils.checkArgsResult('number?', ['any'], undefined, args, app).andThen(_ =>
    ok(nlebool(isNumber(args[0]))))

const realPrim: Prim = (_env, args, app) =>
  Utils.checkArgsResult('real?', ['any'], undefined, args, app).andThen(_ =>
    ok(nlebool(isReal(args[0]))))

const integerPrim: Prim = (_env, args, app) =>
  Utils.checkArgsResult('integer?', ['any'], undefined, args, app).andThen(_ =>
    ok(nlebool(isInteger(args[0]))))

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
  return Utils.checkArgsResult(symbol, ['number?', 'number?'], undefined, args, app).andThen(_ =>
    asNumbers(args).andThen(
      vs => ok(nlebool(op(vs[0], vs[1]))))
  )
}

const ltPrim: Prim = (_env, args, app) => compareOp('<', (x, y) => x < y, args, app)
const leqPrim : Prim = (_env, args, app) => compareOp('<=', (x, y) => x <= y, args, app)
const gtPrim : Prim = (_env, args, app) => compareOp('>', (x, y) => x > y, args, app)
const geqPrim : Prim = (_env, args, app) => compareOp('>=', (x, y) => x >= y, args, app)
const numeqPrim : Prim = (_env, args, app) => compareOp('=', (x, y) => x === y, args, app)

const zeroPrim : Prim = (_env, args, app) =>
  Utils.checkArgsResult('zero?', ['number?'], undefined, args, app).andThen(_ =>
    ok(nlebool(asNum_(args[0]) === 0)))

const positivePrim : Prim = (_env, args, app) =>
  Utils.checkArgsResult('positive?', ['number?'], undefined, args, app).andThen(_ =>
    ok(nlebool(asNum_(args[0]) > 0)))

const negativePrim : Prim = (_env, args, app) =>
  Utils.checkArgsResult('negative?', ['number?'], undefined, args, app).andThen(_ =>
    ok(nlebool(asNum_(args[0]) < 0)))

const oddPrim : Prim = (_env, args, app) =>
  Utils.checkArgsResult('odd?', ['number?'], undefined, args, app).andThen(_ =>
    ok(nlebool((asNum_(args[0]) & 1) === 1)))

const evenPrim : Prim = (_env, args, app) =>
  Utils.checkArgsResult('even?', ['number?'], undefined, args, app).andThen(_ =>
    ok(nlebool((asNum_(args[0]) & 1) !== 1)))

function numericUOp (symbol: string, op: (x: number) => number, args: Exp[], app: Exp): Result<Exp> {
  return Utils.checkArgsResult(symbol, ['number?'], undefined, args, app).andThen(_ =>
    asNumbers(args).andThen(vs => ok(nlenumber(op(vs[0])))))
}

function numericBOp (symbol: string, op: (x: number, y: number) => number, args: Exp[], app: Exp): Result<Exp> {
  return Utils.checkArgsResult(symbol, ['number?', 'number?'], undefined, args, app).andThen(_ =>
    asNumbers(args).andThen(vs => ok(nlenumber(op(vs[0], vs[1])))))
}

function numericNOp (symbol: string, op: (x: number, y: number) => number, args: Exp[], app: Exp): Result<Exp> {
  return Utils.checkArgsResult(symbol, ['number?'], 'number?', args, app).andThen(_ =>
    asNumbers(args).andThen(vs => ok(nlenumber(vs.reduce(op)))))
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
  const argErr = Utils.checkArgsResult('number->string', ['number?'], undefined, args, app)
  const e = args[0]
  return ok(nlestr(asNum_(e).toString()))
}

// TODO: implement:
//   (string->number s)
//   (string->number s radix)

const numericPrimitives: [string, Prim, Doc | undefined][] = [
  ['number?', numberPrim, Docs.number],
  ['real?', realPrim, Docs.real],
  ['integer?', integerPrim, Docs.integer],
  ['<', ltPrim, Docs.lt],
  ['<=', leqPrim, Docs.leq],
  ['>', gtPrim, Docs.gt],
  ['>=', geqPrim, Docs.geq],
  ['=', numeqPrim, Docs.numeq],
  ['zero?', zeroPrim, Docs.zero],
  ['positive?', positivePrim, Docs.positive],
  ['negative?', negativePrim, Docs.negative],
  ['odd?', oddPrim, Docs.odd],
  ['even?', evenPrim, Docs.even],
  ['max', maxPrim, Docs.max],
  ['min', minPrim, Docs.min],
  ['+', plusPrim, Docs.plus],
  ['-', minusPrim, Docs.minus],
  ['*', timesPrim, Docs.times],
  ['/', divPrim, Docs.div],
  ['abs', absPrim, Docs.abs],
  ['modulo', moduloPrim, Docs.modulo],
  ['floor', floorPrim, Docs.floor],
  ['ceiling', ceilingPrim, Docs.ceiling],
  ['truncate', truncatePrim, Docs.truncate],
  ['round', roundPrim, Docs.round],
  ['square', squarePrim, Docs.square],
  ['sqrt', sqrtPrim, Docs.sqrt],
  ['expt', exptPrim, Docs.expt],
  ['number->string', numberStringPrim, Docs.numberString],
]

// Booleans (6.3)

const notPrim: Prim = (_env, args, app) =>
  Utils.checkArgsResult('not', ['boolean?'], 'boolean?', args, app).andThen(_ =>
    ok(nlebool(!asBool_(args[0]))))

const booleanPrim: Prim = (_env, args, app) =>
  Utils.checkArgsResult('boolean?', ['any'], 'boolean?', args, app).andThen(_ =>
    runtimeError(msg('error-arity', 'boolean?', '1', args.length), app))

const booleanPrimitives: [string, Prim, Doc | undefined][] = [
  ['not', notPrim, Docs.not],
  ['boolean?', booleanPrim, Docs.boolean]
]

// Pairs and Lists (6.4)

const pairPrim: Prim = (_env, args, app) =>
  Utils.checkArgsResult('pair?', ['any'], 'boolean?', args, app).andThen(_ =>
    ok(nlebool(isPair(args[0]))))

const consPrim: Prim = (_env, args, app) =>
  Utils.checkArgsResult('cons', ['any', 'any'], 'pair?', args, app).andThen(_ =>
    ok(epair(app.range, args[0], args[1])))

const carPrim : Prim = (_env, args, app) =>
  Utils.checkArgsResult('car', ['pair?'], 'any', args, app).andThen(_ =>
    ok((args[0] as EPair).e1))

const cdrPrim : Prim = (_env, args, app) =>
  Utils.checkArgsResult('cdr', ['pair?'], 'any', args, app).andThen(_ =>
    ok((args[0] as EPair).e2))

// N.B., set-car! and set-cdr! are unimplemented since we only implement the
// pure, functional subset of Scheme.

// TODO: implement caar, cadr, cdar, cddr, caaar, ..., cdddr in some elegant way

const nullPrim : Prim = (_env, args, app) =>
  Utils.checkArgsResult('null?', ['any'], 'boolean?', args, app).andThen(_ =>
    ok(nlebool(args[0].tag === 'nil')))

const listQPrim : Prim = (_env, args, app) =>
  Utils.checkArgsResult('list?', ['any'], 'boolean?', args, app).andThen(_ => 
    ok(nlebool(isList(args[0]))))

const pairListPrimitives: [string, Prim, Doc | undefined][] = [
  ['pair?', pairPrim, Docs.pair],
  ['cons', consPrim, Docs.cons],
  ['car', carPrim, Docs.car],
  ['cdr', cdrPrim, Docs.cdr],
  ['null?', nullPrim, Docs.nullQ],
  ['list?', listQPrim, Docs.listQ]
]

const listPrim: Prim = function (_env, args, app) {
  return ok(arrayToList(args))
}

const makeListPrim: Prim = function (_env, args, app) {
  // N.B., (make-list k) returns the empty list, but this behavior is weird, so we don't replicate it!
  const argErr = Utils.checkArgs('make-list', ['number?', 'any'], undefined, args, app)
  if (argErr) { return argErr }
  const n = asNum_(args[0])
  const fill = args[1]
  let ret: Exp = nlenil()
  for (let i = 0; i < n; i++) {
    ret = nlepair(fill, ret)
  }
  return ok(ret)
}

const lengthPrim: Prim = function (_env, args, app) {
  const argErr = Utils.checkArgs('length', ['list?'], undefined, args, app)
  if (argErr) { return argErr }
  const length = 0
  let e: Exp = args[0]
  while (e.tag !== 'nil') {
    if (e.tag === 'pair') {
      e = e.e2
    } else {
      throw new ICE('lengthPrim', `Processing a non-list that we thought was a list: ${expToString(app)}`)
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
  const argErr = Utils.checkArgs('append', ['list?'], 'list?', args, app)
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
  ['list', listPrim, Docs.list],
  ['make-list', makeListPrim, Docs.makeList],
  ['length', lengthPrim, Docs.length],
  ['append', appendPrim, Docs.append]
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
  Utils.checkArgsResult('string?', ['any'], 'boolean?', args, app).andThen(_ =>
    ok(nlebool(isString(args[0]))))

// TODO: implement:
//   (make-string k)
//   (make-string k char)
//   (string char ...)

const stringLengthPrim: Prim = (_env, args, app) =>
  Utils.checkArgsResult('string-length', ['string?'], undefined, args, app).andThen(_ =>
    ok(nlenumber(asString_(args[0]).length)))

const stringRefPrim: Prim = function (_env, args, app) {
  const argErr = Utils.checkArgs('string-ref', ['string?', 'integer?'], undefined, args, app)
  if (argErr) { return argErr }
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
  ['string?', stringPrim, Docs.stringQ],
  ['string-length', stringLengthPrim, Docs.stringLength],
  ['string-ref', stringRefPrim, Docs.stringRef]
]

// Vectors (6.8)

// N.B., vector operations are unimplemented because they are inherently effectful.

// Bytevectors (6.9)

// N.B., bytevector operations are unimplemented because they are inherently effectful.

// Control features (6.10)

const procedurePrim: Prim = (_env, args, app) =>
  // N.B., once we add non-function primitives, this will need to change.
  Utils.checkArgsResult('procedure?', ['any'], 'boolean?', args, app).andThen(_ =>
    ok(nlebool(isProcedure(args[0]))))

const applyPrim: Prim = (env, args, app) =>
  Utils.checkArgsResult('apply', ['procedure?'], 'any', args, app).andThen(_ =>
    evaluateExp(env, nlecall(args[0], [...args.slice(1)])))

// TODO: for map, do we expand to [f(x1), f(x2), ...] or do we step through
// the full transformation? Probably step through the full transformation, but
// then we also need to be able to step, creating a circularity in our
// dependencies, eek!

// TODO: implement:
//   (string-map fn str1 ... strk)

const mapPrim: Prim = (env, args, app) =>
  Utils.checkArgsResult('map', ['procedure?', 'list?'], 'any', args, app).andThen(_ =>
    evaluateExp(env, arrayToList(unsafeListToArray(args[1]).map(e => nlecall(args[0], [e])))))

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
  ['procedure?', procedurePrim, Docs.procedure],
  ['apply', applyPrim, Docs.apply],
  ['map', mapPrim, Docs.map]
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
].map(v => [v[0], entry(nleprim(v[1]), 'prelude', undefined, v[2])]))