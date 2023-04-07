import * as L from '../lang.js'
import { join, ok, Result } from '../result.js'
import { evaluateExp } from '../evaluator.js'
import { runtimeError } from '../runtime.js'
import * as Utils from './utils.js'
import { msg } from '../messages.js'
import * as Docs from './docs.js'
import * as Pretty from '../pretty.js'
// import { fs } from '../vfs.js'
import { noRange } from '../loc.js'

function asNumbers (args: L.Value[]): Result<number[]> {
  const result = new Array(args.length)
  for (let i = 0; i < args.length; i++) {
    const v = args[i]
    if (L.valueIsNumber(v)) {
      result[i] = v
    } else {
      return runtimeError(msg('error-type-expected', 'number?', v))
    }
  }
  return ok(result)
}

const asValues = (args: L.Value[]): L.Exp[] => args.map(L.nlevalue)

// Equivalence predicates (6.1)

// N.B., don't need these functions:
//   (eqv? x y)
//   (eq? x y)
// Since we don't have effects beside vectors. Therefore, value vs. reference
// equality is not an issue!

const equalPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('equal?', ['any', 'any'], undefined, args, app).andThen(_ =>
    ok(L.valueEquals(args[0], args[1]))))

const equivalencePrimitives: [string, L.Prim, L.Doc | undefined][] = [
  ['equal?', equalPrim, Docs.equal]
]

// Numbers (6.2)

const numberPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('number?', ['any'], undefined, args, app).andThen(_ =>
    ok(L.valueIsNumber(args[0]))))

const realPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('real?', ['any'], undefined, args, app).andThen(_ =>
    ok(L.valueIsReal(args[0]))))

const integerPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('integer?', ['any'], undefined, args, app).andThen(_ =>
    ok(L.valueIsInteger(args[0]))))

// N.B., we don't implement the following functions:
//   (complex? obj)
//   (rational? obj)
//   (exact? z)
//   (inexact? z)
//   (exact->integer? z)
//   (finite? z)
//   (infinite? z)

// Because we only implement the subset of numbers corresponding to the
// Javascript numeric stack: number -> real -> integer

const nanPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('nan?', ['any'], undefined, args, app).andThen(_ =>
    ok(Number.isNaN(args[0] as number))))

function compareOp (symbol: string, op: (x: number, y: number) => boolean, args: L.Value[], app: L.Exp): Promise<Result<L.Value>> {
  return Promise.resolve(Utils.checkArgsResult(symbol, ['number?', 'number?'], undefined, args, app).andThen(_ =>
    asNumbers(args).andThen(
      vs => ok(op(vs[0], vs[1]))))
  )
}

const ltPrim: L.Prim = (_env, args, app) => compareOp('<', (x, y) => x < y, args, app)
const leqPrim : L.Prim = (_env, args, app) => compareOp('<=', (x, y) => x <= y, args, app)
const gtPrim : L.Prim = (_env, args, app) => compareOp('>', (x, y) => x > y, args, app)
const geqPrim : L.Prim = (_env, args, app) => compareOp('>=', (x, y) => x >= y, args, app)
const numeqPrim : L.Prim = (_env, args, app) => compareOp('=', (x, y) => x === y, args, app)

const zeroPrim : L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('zero?', ['number?'], undefined, args, app).andThen(_ =>
    ok(args[0] as number === 0)))

const positivePrim : L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('positive?', ['number?'], undefined, args, app).andThen(_ =>
    ok(args[0] as number > 0)))

const negativePrim : L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('negative?', ['number?'], undefined, args, app).andThen(_ =>
    ok(args[0] as number < 0)))

const oddPrim : L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('odd?', ['integer?'], undefined, args, app).andThen(_ =>
    ok(((args[0] as number) & 1) === 1)))

const evenPrim : L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('even?', ['integer?'], undefined, args, app).andThen(_ =>
    ok(((args[0] as number) & 1) !== 1)))

function numericUOp (symbol: string, op: (x: number) => number, args: L.Value[], app: L.Exp): Promise<Result<L.Value>> {
  return Promise.resolve(Utils.checkArgsResult(symbol, ['number?'], undefined, args, app).andThen(_ =>
    asNumbers(args).andThen(vs => ok(op(vs[0])))))
}

function numericBOp (symbol: string, op: (x: number, y: number) => number, args: L.Value[], app: L.Exp): Promise<Result<L.Value>> {
  return Promise.resolve(Utils.checkArgsResult(symbol, ['number?', 'number?'], undefined, args, app).andThen(_ =>
    asNumbers(args).andThen(vs => ok(op(vs[0], vs[1])))))
}

function numericNOp (symbol: string, op: (x: number, y: number) => number, def: (x: number) => number, args: L.Value[], app: L.Exp): Promise<Result<L.Value>> {
  return Promise.resolve(Utils.checkArgsResult(symbol, ['number?'], 'number?', args, app).andThen(_ =>
    args.length === 1
      ? ok(def(args[0] as number))
      : asNumbers(args).andThen(vs => ok(vs.reduce(op)))))
}

const maxPrim: L.Prim = (_env, args, app) => numericNOp('max', (x, y) => Math.max(x, y), x => x, args, app)
const minPrim: L.Prim = (_env, args, app) => numericNOp('min', (x, y) => Math.min(x, y), x => x, args, app)

const plusPrim: L.Prim = (_env, args, app) => numericNOp('+', (x, y) => x + y, x => x, args, app)
const minusPrim: L.Prim = (_env, args, app) => numericNOp('-', (x, y) => x - y, x => -x, args, app)
const timesPrim: L.Prim = (_env, args, app) => numericNOp('*', (x, y) => x * y, x => x, args, app)
const divPrim: L.Prim = (_env, args, app) => numericNOp('/', (x, y) => x / y, x => 1 / x, args, app)

const absPrim: L.Prim = (_env, args, app) => numericUOp('abs', (x) => Math.abs(x), args, app)

// N.B., not implementing the composite division functions:
//   (floor / n1 n2)
//   (floor-quotient n1 n2)
//   (floor-remainder n1 n2)
//   (truncate/ n1 n2)
//   (truncate-quotient n1 n2)
//   (truncate-remainder n1 n2)
// To avoid clutter in the documentation.

const quotientPrim: L.Prim = (_env, args, app) =>
  numericBOp('quotient', (x, y) => Math.trunc(x / y), args, app)

const remainderPrim: L.Prim = (_env, args, app) =>
  numericBOp('remainder', (x, y) => x % y, args, app)

const moduloPrim: L.Prim = (_env, args, app) =>
  numericBOp('modulo', (x, y) => ((x % y) + y) % y, args, app)

// TODO: implement:
//   (gcd n1 ...)
//   (lcm n1 ...)

// N.B., we don't implement:
//   (numerator q)
//   (denominator q)
// Since we don't implement rationals.

const floorPrim: L.Prim = (_env, args, app) => numericUOp('floor', (x) => Math.floor(x), args, app)
const ceilingPrim: L.Prim = (_env, args, app) => numericUOp('ceiling', (x) => Math.ceil(x), args, app)
const truncatePrim: L.Prim = (_env, args, app) => numericUOp('truncate', (x) => Math.trunc(x), args, app)
const roundPrim: L.Prim = (_env, args, app) => numericUOp('round', (x) => Math.round(x), args, app)

// N.B., we don't implement:
//   (rationalize x y)
// Because we don't implement rationals.

const squarePrim: L.Prim = (_env, args, app) => numericUOp('square', (x) => Math.pow(x, 2), args, app)
const sqrtPrim: L.Prim = (_env, args, app) => numericUOp('sqrt', (x) => Math.sqrt(x), args, app)

// N.B., we don't implement:
//   (exact-integer-sqrt k)
// To avoid polluting the documentation.

const exptPrim: L.Prim = (_env, args, app) => numericBOp('expt', (x, y) => Math.pow(x, y), args, app)

// N.B., we don't implement:
//   (make-rectangular x1 x2)   ...probably not!
//   (make-polar x3 x4)         ...probably not!
//   (real-part z)              ...probably not!
//   (imag-part z)              ...probably not!
//   (magnitude z)              ...probably not!
//   (angle z)                  ...probably not!
// Because we don't implement complex numbers.

const numberStringPrim: L.Prim = (_env, args, app) => {
  // N.B., we don't support (number->string z radix)---no need at this opint.
  const argErr = Utils.checkArgs('number->string', ['number?'], undefined, args, app)
  if (argErr) { return Promise.resolve(argErr) }
  return Promise.resolve(ok((args[0] as number).toString()))
}

// TODO: implement:
//   (string->number s)
//   (string->number s radix)

const stringNumberPrim: L.Prim = (_env, args, app) => {
  const argErr = Utils.checkArgs('string->number', ['string?'], undefined, args, app)
  if (argErr) { return Promise.resolve(argErr) }
  const s = args[0] as string
  if (/^[+-]?\d+$/.test(s)) {
    return Promise.resolve(ok(parseInt(s)))
  } else if (/^[+-]?(\d+|(\d*\.\d+)|(\d+\.\d*))([eE][+-]?\d+)?$/.test(s)) {
    return Promise.resolve(ok(parseFloat(s)))
  } else {
    return Promise.resolve(runtimeError(msg('error-runtime-parsing', 'string->number', Pretty.expToString(0, L.nlevalue(args[0])), 'number'), app))
  }
}

// Additional functions from racket/base

const expPrim: L.Prim = (_env, args, app) =>
  numericUOp('exp', (x) => Math.exp(x), args, app)

const logPrim: L.Prim = (_env, args, app) =>
  numericUOp('log', (x) => Math.log(x), args, app)

const sinPrim: L.Prim = (_env, args, app) =>
  numericUOp('sin', (x) => Math.sin(x), args, app)

const cosPrim: L.Prim = (_env, args, app) =>
  numericUOp('cos', (x) => Math.cos(x), args, app)

const tanPrim: L.Prim = (_env, args, app) =>
  numericUOp('tan', (x) => Math.tan(x), args, app)

const asinPrim: L.Prim = (_env, args, app) =>
  numericUOp('asin', (x) => Math.asin(x), args, app)

const acosPrim: L.Prim = (_env, args, app) =>
  numericUOp('acos', (x) => Math.acos(x), args, app)

const atanPrim: L.Prim = (_env, args, app) =>
  numericUOp('atan', (x) => Math.atan(x), args, app)

const equalsEpsPrim: L.Prim = (env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('=-eps', ['number?'], undefined, args, app).asyncAndThen(async _ => {
    return await evaluateExp(env, L.nlelam(['x', 'y'], L.nlecall(L.nlevar('<='), [
      L.nlecall(L.nlevar('abs'), [
        L.nlecall(L.nlevar('-'), [
          L.nlevar('x'), L.nlevar('y')
        ])
      ]),
      L.nlevalue(args[0])])))
  }))

const numericPrimitives: [string, L.Prim, L.Doc | undefined][] = [
  ['number?', numberPrim, Docs.number],
  ['real?', realPrim, Docs.real],
  ['integer?', integerPrim, Docs.integer],
  ['nan?', nanPrim, Docs.nanQ],
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
  ['⋅', timesPrim, Docs.times],
  ['/', divPrim, Docs.div],
  ['abs', absPrim, Docs.abs],
  ['quotient', quotientPrim, Docs.quotient],
  ['remainder', remainderPrim, Docs.remainder],
  ['modulo', moduloPrim, Docs.modulo],
  ['floor', floorPrim, Docs.floor],
  ['ceiling', ceilingPrim, Docs.ceiling],
  ['truncate', truncatePrim, Docs.truncate],
  ['round', roundPrim, Docs.round],
  ['square', squarePrim, Docs.square],
  ['sqrt', sqrtPrim, Docs.sqrt],
  ['expt', exptPrim, Docs.expt],
  ['number->string', numberStringPrim, Docs.numberString],
  ['string->number', stringNumberPrim, Docs.stringNumber],
  ['exp', expPrim, Docs.exp],
  ['log', logPrim, Docs.log],
  ['sin', sinPrim, Docs.sin],
  ['cos', cosPrim, Docs.cos],
  ['tan', tanPrim, Docs.tan],
  ['asin', asinPrim, Docs.asin],
  ['acos', acosPrim, Docs.acos],
  ['atan', atanPrim, Docs.atan],
  ['=-eps', equalsEpsPrim, Docs.equalsEps]
]

// Booleans (6.3)

const notPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('not', ['boolean?'], undefined, args, app).andThen(_ =>
    ok(!(args[0] as boolean))))

const booleanPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('boolean?', ['any'], undefined, args, app).andThen(_ =>
    ok(L.valueIsBoolean(args[0]))))

// From racket/base

const nandPrim: L.Prim = (env, args, app) =>
  Utils.checkArgsResult('nand', [], 'boolean?', args, app).asyncAndThen(_ =>
    evaluateExp(env, L.nlecall(L.nlevar('not'), [L.nleand(asValues(args))])))

const norPrim: L.Prim = (env, args, app) =>
  Utils.checkArgsResult('nand', [], 'boolean?', args, app).asyncAndThen(_ =>
    evaluateExp(env, L.nlecall(L.nlevar('not'), [L.nleor(asValues(args))])))

const impliesPrim: L.Prim = (env, args, app) =>
  Utils.checkArgsResult('implies', ['boolean?', 'boolean?'], undefined, args, app).asyncAndThen(_ =>
    evaluateExp(env, L.nleif(L.nlevalue(args[0]), L.nlevalue(args[1]), L.nlebool(true))))

const xorPrim: L.Prim = (env, args, app) =>
  Utils.checkArgsResult('xor', ['boolean?', 'boolean?'], undefined, args, app).asyncAndThen(_ =>
    evaluateExp(env, L.nleor([
      L.nleand([L.nlevalue(args[0]), L.nlecall(L.nlevar('not'), [L.nlevalue(args[1])])]),
      L.nleand([L.nlecall(L.nlevar('not'), [L.nlevalue(args[0])]), L.nlevalue(args[1])])
    ])))

const booleanPrimitives: [string, L.Prim, L.Doc | undefined][] = [
  ['not', notPrim, Docs.not],
  ['boolean?', booleanPrim, Docs.boolean],
  ['nand', nandPrim, Docs.nand],
  ['nor', norPrim, Docs.nor],
  ['implies', impliesPrim, Docs.implies],
  ['xor', xorPrim, Docs.xor]
]

// Pairs and Lists (6.4)

const pairQPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('pair?', ['any'], 'boolean?', args, app).andThen(_ =>
    ok(L.valueIsPair(args[0]))))

const consPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('cons', ['any', 'any'], 'pair?', args, app).andThen(_ =>
    ok(L.vpair(args[0], args[1]))))

const pairPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('pair', ['any', 'any'], 'pair?', args, app).andThen(_ =>
    ok(L.vpair(args[0], args[1]))))

const carPrim : L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('car', ['pair?'], 'any', args, app).andThen(_ =>
    ok((args[0] as L.PairType).fst)))

const cdrPrim : L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('cdr', ['pair?'], 'any', args, app).andThen(_ =>
    ok((args[0] as L.PairType).snd)))

// N.B., set-car! and set-cdr! are unimplemented since we only implement the
// pure, functional subset of Scheme.

// TODO: implement caar, cadr, cdar, cddr, caaar, ..., cdddr in some elegant way

const nullPrim : L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('null?', ['any'], 'boolean?', args, app).andThen(_ =>
    ok(L.valueIsNull(args[0]))))

const listQPrim : L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('list?', ['any'], 'boolean?', args, app).andThen(_ =>
    ok(L.valueIsList(args[0]))))

const pairListPrimitives: [string, L.Prim, L.Doc | undefined][] = [
  ['pair?', pairQPrim, Docs.pairQ],
  ['cons', consPrim, Docs.cons],
  ['pair', pairPrim, Docs.pair],
  ['car', carPrim, Docs.car],
  ['cdr', cdrPrim, Docs.cdr],
  ['null?', nullPrim, Docs.nullQ],
  ['list?', listQPrim, Docs.listQ]
]

const listPrim: L.Prim = (_env, args, app) => Promise.resolve(ok(L.valueArrayToList(args)))

const makeListPrim: L.Prim = (_env, args, app) => {
  // N.B., (make-list k) returns the empty list, but this behavior is weird, so we don't replicate it!
  const argErr = Utils.checkArgs('make-list', ['number?', 'any'], undefined, args, app)
  if (argErr) { return Promise.resolve(argErr) }
  const n = args[0] as number
  const fill = args[1]
  let ret: L.Value = null
  for (let i = 0; i < n; i++) {
    ret = L.vpair(fill, ret)
  }
  return Promise.resolve(ok(ret))
}

const lengthPrim: L.Prim = (_env, args, app) => {
  const argErr = Utils.checkArgs('length', ['list?'], undefined, args, app)
  if (argErr) { return Promise.resolve(argErr) }
  let length = 0
  let e: L.Value = args[0]
  while (!L.valueIsNull(e)) {
    length += 1
    e = (e as L.PairType).snd
  }
  return Promise.resolve(ok(length))
}

function appendOne_ (l1: L.Value, l2: L.Value): L.Value {
  if (L.valueIsNull(l1)) {
    return l2
  } else {
    const p1 = l1 as L.PairType
    return L.vpair(p1.fst, appendOne_(p1.snd, l2))
  }
}

const appendPrim: L.Prim = (_env, args, app) => {
  const argErr = Utils.checkArgs('append', ['list?'], 'list?', args, app)
  if (argErr) { return Promise.resolve(argErr) }
  let ret = args[0]
  for (let i = 1; i < args.length; i++) {
    ret = appendOne_(ret, args[i])
  }
  return Promise.resolve(ok(ret))
}

const reversePrim: L.Prim = (_env, args, app) => {
  const argErr = Utils.checkArgs('reverse', ['list?'], 'list?', args, app)
  if (argErr) { return Promise.resolve(argErr) }
  const queue = []
  let v = args[0]
  while (!L.valueIsNull(v)) {
    queue.push(v)
    v = (v as L.PairType).snd
  }
  queue.reverse()
  let ret: L.Value = null
  while (queue.length > 0) {
    const next = queue.pop() as L.PairType
    ret = L.vpair(next.fst, ret)
  }
  return Promise.resolve(ok(ret))
}

const listTailPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('list-tail', ['list?', 'number?'], 'list?', args, app).andThen(_ => {
    const list = L.valueListToArray_(args[0])
    const k = args[1] as number
    if (k < 0 || k > list.length) {
      return runtimeError(msg('error-precondition-not-met', 'list-tail', 2, '<= length of list', k), app)
    }
    return ok(L.valueArrayToList(list.slice(k)))
  }))

const listTakePrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('list-take', ['list?', 'number?'], 'list?', args, app).andThen(_ => {
    const list = L.valueListToArray_(args[0])
    const k = args[1] as number
    if (k < 0 || k > list.length) {
      return runtimeError(msg('error-precondition-not-met', 'list-take', 2, '<= length of list', k), app)
    }
    return ok(L.valueArrayToList(list.slice(0, k)))
  }))

const listRefPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('list-ref', ['list?', 'number?'], 'any', args, app).andThen(_ => {
    const list = L.valueListToArray_(args[0])
    const i = args[1] as number
    if (i < 0 || i >= list.length) {
      return runtimeError(msg('error-precondition-not-met', 'list-ref', 2, 'valid index into list', i), app)
    }
    return ok(list[i])
  }))

// N.B., list-set! is unimplemented since it is effectful.

// N.B., association list operations implemented below.
//   (memq obj list)
//   (memv obj list)
//   (member obj list compare)
//   (assq obj alist)
//   (assv obj alist)
//   (assoc obj alist)
//   (assoc obj alist compare)

// N.B., no need for list-copy since we have immutable lists.
//   (list-copy obj)

// Other list functions

const indexOfPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('index-of', ['list?', 'any'], 'number?', args, app).andThen(_ => {
    const list = L.valueListToArray_(args[0])
    for (let i = 0; i < list.length; i++) {
      if (L.valueEquals(list[i], args[1])) {
        return ok(i)
      }
    }
    return ok(-1)
  }))

const assocKeyPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('assoc-key?', ['any', 'list?'], undefined, args, app).andThen(_ => {
    const v = args[0]
    const list = L.valueListToArray_(args[1])
    for (let i = 0; i < list.length; i++) {
      if (L.valueIsPair(list[i])) {
        const pair = list[i] as L.PairType
        if (L.valueEquals(pair.fst, v)) {
          return ok(true)
        }
      } else {
        return runtimeError(msg('error-precondition-not-met', 'assoc-key?', 2, 'list of pairs', Pretty.expToString(0, L.nlevalue(args[1])), app))
      }
    }
    return ok(false)
  }))

const assocRefPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('assoc-ref', ['any', 'list?'], undefined, args, app).andThen(_ => {
    const v = args[0]
    const list = L.valueListToArray_(args[1])
    for (let i = 0; i < list.length; i++) {
      if (L.valueIsPair(list[i])) {
        const pair = list[i] as L.PairType
        if (L.valueEquals(pair.fst, v)) {
          return ok(pair.snd)
        }
      } else {
        return runtimeError(msg('error-precondition-not-met', 'assoc-ref', 2, 'list of pairs', Pretty.expToString(0, L.nlevalue(args[1])), app))
      }
    }
    return runtimeError(msg('error-assoc-not-found', Pretty.expToString(0, L.nlevalue(v)), Pretty.expToString(0, L.nlevalue(args[1]))), app)
  }))

const assocSetPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('assoc-set', ['any', 'any', 'list?'], undefined, args, app).andThen(_ => {
    const k = args[0]
    const v = args[1]
    const list = L.valueListToArray_(args[2])
    // Update the key in-place if possible.
    for (let i = 0; i < list.length; i++) {
      if (L.valueIsPair(list[i])) {
        const pair = list[i] as L.PairType
        if (L.valueEquals(pair.fst, k)) {
          const ret = [...list]
          ret[i] = L.vpair(k, v)
          return ok(L.valueArrayToList(ret))
        }
      } else {
        return runtimeError(msg('error-precondition-not-met', 'assoc-ref', 2, 'list of pairs', Pretty.expToString(0, L.nlevalue(args[1])), app))
      }
    }
    // Otherwise, we didn't find the key. Add it to our list.
    const ret = L.valueArrayToList([...list, L.vpair(k, v)])
    return ok(ret)
  }))

const listPrimitives: [string, L.Prim, L.Doc | undefined][] = [
  ['list', listPrim, Docs.list],
  ['make-list', makeListPrim, Docs.makeList],
  ['length', lengthPrim, Docs.length],
  ['append', appendPrim, Docs.append],
  ['reverse', reversePrim, Docs.reverse],
  ['list-tail', listTailPrim, Docs.listTail],
  ['list-drop', listTailPrim, Docs.listDrop],
  ['list-take', listTakePrim, Docs.listTake],
  ['list-ref', listRefPrim, Docs.listRef],
  ['index-of', indexOfPrim, Docs.indexOf],
  ['assoc-key?', assocKeyPrim, Docs.assocKey],
  ['assoc-ref', assocRefPrim, Docs.assocRef],
  ['assoc-set', assocSetPrim, Docs.assocSet]
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

const charQPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('char?', ['any'], undefined, args, app).andThen(_ =>
    ok(L.valueIsChar(args[0]))))

function pairwiseSatisfies<T> (f: (a: T, b: T) => boolean, xs: T[]): boolean {
  if (xs.length <= 1) {
    return true
  } else {
    for (let i = 0; i < xs.length - 1; i++) {
      if (!f(xs[i], xs[i + 1])) {
        return false
      }
    }
    return true
  }
}

function mkCharComparePrim (name: string, f: (a: string, b: string) => boolean): L.Prim {
  return (_env, args, app) => {
    const err = Utils.checkArgs(name, [], 'char?', args, app)
    if (err) { return Promise.resolve(err) }
    return Promise.resolve(ok(pairwiseSatisfies((a, b) => f(L.valueGetChar_(a), L.valueGetChar_(b)), args)))
  }
}

const charEqPrim: L.Prim = mkCharComparePrim('char=?', (a, b) => a === b)
const charLtPrim: L.Prim = mkCharComparePrim('char<?', (a, b) => a.codePointAt(0)! < b.codePointAt(0)!)
const charGtPrim: L.Prim = mkCharComparePrim('char>?', (a, b) => a.codePointAt(0)! > b.codePointAt(0)!)
const charLeqPrim: L.Prim = mkCharComparePrim('char<=?', (a, b) => a.codePointAt(0)! <= b.codePointAt(0)!)
const charGeqPrim: L.Prim = mkCharComparePrim('char>=?', (a, b) => a.codePointAt(0)! >= b.codePointAt(0)!)
const charEqCiPrim: L.Prim = mkCharComparePrim('char-ci=?', (a, b) => a.toLowerCase() === b.toLowerCase())
const charLtCiPrim: L.Prim = mkCharComparePrim('char-ci<?', (a, b) => a.toLowerCase().codePointAt(0)! < b.toLowerCase().codePointAt(0)!)
const charGtCiPrim: L.Prim = mkCharComparePrim('char-ci>?', (a, b) => a.toLowerCase().codePointAt(0)! > b.toLowerCase().codePointAt(0)!)
const charLeqCiPrim: L.Prim = mkCharComparePrim('char-ci<=?', (a, b) => a.toLowerCase().codePointAt(0)! <= b.toLowerCase().codePointAt(0)!)
const charGeqCiPrim: L.Prim = mkCharComparePrim('char-ci>=?', (a, b) => a.toLowerCase().codePointAt(0)! >= b.toLowerCase().codePointAt(0)!)

function mkCharPredicatePrim (name: string, f: (a: string) => boolean): L.Prim {
  return (_env, args, app) => {
    const err = Utils.checkArgs(name, ['char?'], undefined, args, app)
    if (err) { return Promise.resolve(err) }
    return Promise.resolve(ok(f(L.valueGetChar_(args[0]))))
  }
}

const charAlphabeticPrim: L.Prim =
  mkCharPredicatePrim('char-alphabetic?', (a) => /\p{L}/gu.test(a))
const charNumericPrim: L.Prim =
  mkCharPredicatePrim('char-numeric?', (a) => /\p{N}/gu.test(a))
const charWhitespacePrim: L.Prim =
  mkCharPredicatePrim('char-whitespace?', (a) => /\p{Z}/gu.test(a))
const charUpperCasePrim: L.Prim =
  mkCharPredicatePrim('char-upper-case?', (a) => /\p{Lu}/gu.test(a))
const charLowerCasePrim: L.Prim =
  mkCharPredicatePrim('char-lower-case?', (a) => /\p{Ll}/gu.test(a))

const digitValuePrim: L.Prim = (_env, args, app) => {
  const err = Utils.checkArgs('digit-value', ['char?'], undefined, args, app)
  if (err) { return Promise.resolve(err) }
  const char = L.valueGetChar_(args[0])
  const n = parseInt(char, 10)
  if (isNaN(n)) {
    return Promise.resolve(runtimeError(msg('error-precondition-not-met', 'digit-value', 1, 'decimal digit', char), app))
  } else {
    return Promise.resolve(ok(n))
  }
}

const charIntegerPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('char->integer', ['char?'], undefined, args, app).andThen(_ =>
    ok(L.valueGetChar_(args[0]).codePointAt(0)!)))

const integerCharPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('integer->char', ['integer?'], undefined, args, app).andThen(_ =>
    ok(L.vchar(String.fromCodePoint(args[0] as number)))))

const charUpcasePrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('char-upcase', ['char?'], undefined, args, app).andThen(_ =>
    ok(L.vchar(L.valueGetChar_(args[0]).toUpperCase()))))

const charDowncasePrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('char-downcase', ['char?'], undefined, args, app).andThen(_ =>
    ok(L.vchar(L.valueGetChar_(args[0]).toLowerCase()))))

// N.B., "folding" in Unicode returns a character to a "canonical" form, suitable for
// comparison in a "case-insensitive" manner. toLowerCase is Unicode aware, so maybe
// this implementation works. But... yea, maybe not!
//
// See: https://unicode.org/reports/tr18/#General_Category_Property
const charFoldcasePrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('char-foldcase', ['char?'], 'char?', args, app).andThen(_ =>
    ok(L.vchar(L.valueGetChar_(args[0]).toLowerCase()))))

// Strings (6.7)

const stringQPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('string?', ['any'], 'boolean?', args, app).andThen(_ =>
    ok(L.valueIsString(args[0]))))

// N.B., we don't implement the (make-string k) variant because our strings are
// immutable, so having an "empty" string of size k does not make sense.
const makeStringPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('make-string', ['integer?', 'char?'], 'string?', args, app).andThen(_ =>
    ok(L.valueGetChar_(args[1]).repeat(args[0] as number))))

const stringPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('string', ['char?'], 'char?', args, app).andThen(_ =>
    ok(args.map((e) => L.valueGetChar_(e)).join(''))))

const stringLengthPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('string-length', ['string?'], undefined, args, app).andThen(_ =>
    ok((args[0] as string).length)))

const stringRefPrim: L.Prim = (_env, args, app) => {
  const argErr = Utils.checkArgs('string-ref', ['string?', 'integer?'], undefined, args, app)
  if (argErr) { return Promise.resolve(argErr) }
  const str = args[0] as string
  const i = args[1] as number
  if (i >= 0 && i < str.length) {
    return Promise.resolve(ok(L.vchar(str[i])))
  } else {
    return Promise.resolve(runtimeError(msg('error-index-string', i, str), app))
  }
}

// N.B., string-set! is unimplemented since it is effectful.

function mkStringComparePrim (name: string, f: (a: string, b: string) => boolean): L.Prim {
  return (_env, args, app) =>
    Promise.resolve(Utils.checkArgsResult(name, [], 'string?', args, app).andThen(_ =>
      ok(pairwiseSatisfies((a, b) => f(a as string, b as string), args))))
}

const stringEqPrim: L.Prim = mkStringComparePrim('string=?', (a, b) => a === b)
const stringLtPrim: L.Prim = mkStringComparePrim('string<?', (a, b) => a < b)
const stringGtPrim: L.Prim = mkStringComparePrim('string>?', (a, b) => a > b)
const stringLeqPrim: L.Prim = mkStringComparePrim('string<=?', (a, b) => a <= b)
const stringGeqPrim: L.Prim = mkStringComparePrim('string>=?', (a, b) => a >= b)
const stringEqCiPrim: L.Prim = mkStringComparePrim('string-ci=?', (a, b) => a.toLowerCase() === b.toLowerCase())
const stringLtCiPrim: L.Prim = mkStringComparePrim('string-ci<?', (a, b) => a.toLowerCase() < b.toLowerCase())
const stringGtCiPrim: L.Prim = mkStringComparePrim('string-ci>?', (a, b) => a.toLowerCase() > b.toLowerCase())
const stringLeqCiPrim: L.Prim = mkStringComparePrim('string-ci<=?', (a, b) => a.toLowerCase() <= b.toLowerCase())
const stringGeqCiPrim: L.Prim = mkStringComparePrim('string-ci>=?', (a, b) => a.toLowerCase() >= b.toLowerCase())

const stringUpcasePrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('string-upcase', ['string?'], undefined, args, app).andThen(_ =>
    ok((args[0] as string).toUpperCase())))

const stringDowncasePrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('string-downcase', ['string?'], undefined, args, app).andThen(_ =>
    ok((args[0] as string).toLowerCase())))

const stringFoldcasePrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('string-foldcase', ['string?'], undefined, args, app).andThen(_ =>
    ok((args[0] as string).toLowerCase())))

const substringPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('substring', ['string?', 'integer?', 'integer?'], undefined, args, app).andThen(_ =>
    ok((args[0] as string).substring(args[1] as number, args[2] as number))))

const stringAppendPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('string-append', ['string?'], 'string?', args, app).andThen(_ =>
    ok((args as string[]).join(''))))

const stringListPrim: L.Prim = (_env, args, app) => {
  if (args.length !== 1 && args.length !== 3) {
    return Promise.resolve(runtimeError(msg('error-arity', 'string->list', '1 or 3', args.length), app))
  }
  if (!L.valueIsString(args[0])) {
    return Promise.resolve(runtimeError(msg('error-type-expected-fun', 1, 'string->list', 'string', L.nlevalue(args[0])), app))
  }
  const str = args[0] as string
  let start, end
  if (args.length === 1) {
    start = 0
    end = str.length
  } else {
    if (!L.valueIsInteger(args[1])) {
      return Promise.resolve(runtimeError(msg('error-type-expected-fun', 2, 'string->list', 'integer', L.nlevalue(args[1])), app))
    }
    if (!L.valueIsInteger(args[2])) {
      return Promise.resolve(runtimeError(msg('error-type-expected-fun', 3, 'string->list', 'integer', L.nlevalue(args[2])), app))
    }
    start = args[1] as number
    end = args[2] as number
  }
  return Promise.resolve(ok(L.valueArrayToList(str.substring(start, end).split('').map(L.vchar))))
}

const listStringPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('list->string', ['list?'], undefined, args, app).andThen(_ => {
    const lst = L.valueListToArray_(args[0])
    for (const e of lst) {
      if (!L.valueIsChar(e)) {
        return runtimeError(msg('error-type-expected-fun',
          'list->string', 1, 'list of chars', e), app)
      }
    }
    return ok(lst.map(L.valueGetChar_).join(''))
  }))

const stringVectorPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('string->vector', ['string?'], undefined, args, app).andThen(_ =>
    ok((args[0] as string).split('').map(L.vchar))))

const vectorStringPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('vector->string', ['vector?'], undefined, args, app).andThen(_ => {
    const vec = args[0] as L.Value[]
    for (const e of vec) {
      if (!L.valueIsChar(e)) {
        return runtimeError(msg('error-type-expected-fun',
          'vector->string', 1, 'vector of chars', e), app)
      }
    }
    return ok(vec.map(L.valueGetChar_).join(''))
  }))

// N.B., the following functions:
//
//   (string-copy string)
//   (string-copy string start)
//   (string-copy string start end)
//
// and string-copy! and string-fill! are unimplemented since they don't make
// sense in an immutable context.

// Additional functions from racket/string.

const stringContainsPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('string-contains', ['string?', 'string?'], undefined, args, app).andThen(_ =>
    ok((args[1] as string).includes(args[0] as string))))

const stringSplitPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('string-split', ['string?', 'string?'], undefined, args, app).andThen(_ =>
    ok(L.valueArrayToList((args[0] as string).split(args[1] as string)))))

const stringSplitVectorPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('string-split', ['string?', 'string?'], undefined, args, app).andThen(_ =>
    ok((args[0] as string).split(args[1] as string))))

// const fileStringPrim: L.Prim = (_env, args, app) =>
//   Utils.checkArgsResult('file->string', ['string?'], undefined, args, app).asyncAndThen(async _ =>
//     (await fs.read(args[0] as string)).asyncAndThen(s =>
//       Promise.resolve(ok(s))))

// const fileLinesPrim: L.Prim = (env, args, app) =>
//   Utils.checkArgsResult('file->lines', ['string?'], undefined, args, app).asyncAndThen(_ =>
//     evaluateExp(env, L.nlecall(L.nlevar('string-split'), [
//       L.nlecall(L.nlevar('file->string'), [L.nlevalue(args[0])]),
//       L.nlestr('\n')
//     ])))

export type ReactiveFile = { renderAs: 'reactive-file', callback: L.FunctionType }
export const reactiveFile = (callback: L.FunctionType): ReactiveFile => ({ renderAs: 'reactive-file', callback })

const withFile: L.Prim = (env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('with-file', ['procedure?'], undefined, args, app).andThen(_ =>
    ok(reactiveFile(args[0] as L.FunctionType))))

const stringPrimitives: [string, L.Prim, L.Doc | undefined][] = [
  ['char?', charQPrim, Docs.charQ],
  ['char=?', charEqPrim, Docs.charEq],
  ['char<?', charLtPrim, Docs.charLt],
  ['char>?', charGtPrim, Docs.charGt],
  ['char<=?', charLeqPrim, Docs.charLeq],
  ['char>=?', charGeqPrim, Docs.charGeq],
  ['char-ci=?', charEqCiPrim, Docs.charEqCi],
  ['char-ci<?', charLtCiPrim, Docs.charLtCi],
  ['char-ci>?', charGtCiPrim, Docs.charGtCi],
  ['char-ci<=?', charLeqCiPrim, Docs.charLeqCi],
  ['char-ci>=?', charGeqCiPrim, Docs.charGeqCi],
  ['char-alphabetic?', charAlphabeticPrim, Docs.charAlphabetic],
  ['char-numeric?', charNumericPrim, Docs.charNumeric],
  ['char-whitespace?', charWhitespacePrim, Docs.charWhitespace],
  ['char-upper-case?', charUpperCasePrim, Docs.charUpperCase],
  ['char-lower-case?', charLowerCasePrim, Docs.charLowerCase],
  ['digit-value', digitValuePrim, Docs.digitValue],
  ['char->integer', charIntegerPrim, Docs.charToInteger],
  ['integer->char', integerCharPrim, Docs.integerToChar],
  ['char-upcase', charUpcasePrim, Docs.charUpcase],
  ['char-downcase', charDowncasePrim, Docs.charDowncase],
  ['char-foldcase', charFoldcasePrim, Docs.charFoldcase],
  ['string?', stringQPrim, Docs.stringQ],
  ['make-string', makeStringPrim, Docs.makeString],
  ['string', stringPrim, Docs.string],
  ['string-length', stringLengthPrim, Docs.stringLength],
  ['string-ref', stringRefPrim, Docs.stringRef],
  ['string=?', stringEqPrim, Docs.stringEq],
  ['string<?', stringLtPrim, Docs.stringLt],
  ['string>?', stringGtPrim, Docs.stringGt],
  ['string<=?', stringLeqPrim, Docs.stringLeq],
  ['string>=?', stringGeqPrim, Docs.stringGeq],
  ['string-ci=?', stringEqCiPrim, Docs.stringEqCi],
  ['string-ci<?', stringLtCiPrim, Docs.stringLtCi],
  ['string-ci>?', stringGtCiPrim, Docs.stringGtCi],
  ['string-ci<=?', stringLeqCiPrim, Docs.stringLeqCi],
  ['string-ci>=?', stringGeqCiPrim, Docs.stringGeqCi],
  ['string-upcase', stringUpcasePrim, Docs.stringUpcase],
  ['string-downcase', stringDowncasePrim, Docs.stringDowncase],
  ['string-foldcase', stringFoldcasePrim, Docs.stringFoldcase],
  ['substring', substringPrim, Docs.substring],
  ['string->list', stringListPrim, Docs.stringList],
  ['list->string', listStringPrim, Docs.listString],
  ['string->vector', stringVectorPrim, Docs.stringVector],
  ['vector->string', vectorStringPrim, Docs.vectorString],
  ['string-contains', stringContainsPrim, Docs.stringContains],
  ['string-split', stringSplitPrim, Docs.stringSplit],
  ['string-split-vector', stringSplitVectorPrim, Docs.stringSplitVector],
  ['string-append', stringAppendPrim, Docs.stringAppend],
  // ['file->string', fileStringPrim, Docs.fileString],
  // ['file->lines', fileLinesPrim, Docs.fileLines]
  ['with-file', withFile, Docs.withFile]
]

// Vectors (6.8)

const vectorQPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('vector?', ['any'], undefined, args, app).andThen(_ =>
    ok(L.valueIsVector(args[0]))))

const vectorPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('vector', [], 'any', args, app).andThen(_ =>
    // N.B., make a shallow copy of the array so that mutation doesn't make things weird.
    ok([...args])))

const makeVectorPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('make-vector', ['integer?', 'any'], undefined, args, app).andThen(_ => {
    const n = args[0] as number
    if (n < 0) {
      return runtimeError(msg('error-precondition-not-met', 'make-vector', '1', 'non-negative', n), app)
    }
    const ret = new Array<L.Value>(n)
    for (let i = 0; i < n; i++) {
      ret[i] = args[1]
    }
    return ok(ret)
  }))

const vectorLengthPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('vector-length', ['vector?'], undefined, args, app).andThen(_ =>
    ok((args[0] as L.Value[]).length)))

const vectorRefPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('vector-ref', ['vector?', 'integer?'], undefined, args, app).andThen(_ => {
    const vec = args[0] as L.Value[]
    const i = args[1] as number
    if (i < 0 || i >= vec.length) {
      return runtimeError(msg('error-index-vector', i, Pretty.valueToString(0, vec)), app)
    } else {
      return ok(vec[i])
    }
  }))

const vectorSetPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('vector-set!', ['vector?', 'integer?', 'any'], undefined, args, app).andThen(_ => {
    const vec = args[0] as L.Value[]
    const i = args[1] as number
    if (i < 0 || i >= vec.length) {
      return runtimeError(msg('error-index-vector', i, Pretty.valueToString(0, vec)), app)
    } else {
      vec[i] = args[2]
      return ok(undefined)
    }
  }))

const vectorFillPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('vector-fill!', ['vector?', 'any'], undefined, args, app).andThen(_ => {
    const vec = args[0] as L.Value[]
    const v = args[1]
    for (let i = 0; i < vec.length; i++) {
      vec[i] = v
    }
    return ok(undefined)
  }))

const vectorListPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('vector->list', ['vector?'], undefined, args, app).andThen(_ =>
    ok(L.valueArrayToList(args[0] as L.Value[]))))

const listVectorPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('list->vector', ['list?'], undefined, args, app).andThen(_ =>
    ok(L.valueListToArray_(args[0] as L.Value[]))))

// N.B., this is identical to rangePrim except it does not conver the output
// to a list! Probably should refactor this...
const vectorRangePrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('vector-range', [], 'number?', args, app).andThen(_ => {
    if (args.length === 0 || args.length > 3) {
      return runtimeError(msg('error-arity', 'vector-range', '1--3', args.length), app)
    } else {
      const m = args.length === 1 ? 0 : args[0] as number
      const n = args.length === 1 ? args[0] as number : args[1] as number
      const step = args.length < 3 ? 1 : args[2] as number
      const arr = []
      // N.B., to prevent the internal infinite loop that would result
      // from having a zero step.
      if (step === 0) {
        return runtimeError(msg('error-precondition-not-met', 'vector-range', '3', 'non-zero', step), app)
      }
      for (let i = m; step > 0 ? i < n : i > n; i += step) {
        arr.push(i)
      }
      return ok(arr)
    }
  }))

const vectorAppendPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('vector-append', [], 'vector?', args, app).andThen(_ => {
    if (args.length === 0) {
      return ok([])
    }
    const vecs = args as L.Value[][]
    const arr = new Array<L.Value>(vecs.map(v => v.length).reduce((a, b) => a + b))
    let i = 0
    for (let j = 0; j < vecs.length; j++) {
      for (let k = 0; k < vecs[j].length; k++) {
        arr[i++] = vecs[j][k]
      }
    }
    return ok(arr)
  }))

const vectorPrimitives: [string, L.Prim, L.Doc][] = [
  ['vector?', vectorQPrim, Docs.vectorQ],
  ['vector', vectorPrim, Docs.vector],
  ['make-vector', makeVectorPrim, Docs.makeVector],
  ['vector-length', vectorLengthPrim, Docs.vectorLength],
  ['vector-ref', vectorRefPrim, Docs.vectorRef],
  ['vector-set!', vectorSetPrim, Docs.vectorSet],
  ['vector-fill!', vectorFillPrim, Docs.vectorFill],
  ['vector->list', vectorListPrim, Docs.vectorList],
  ['list->vector', listVectorPrim, Docs.listVector],
  ['vector-range', vectorRangePrim, Docs.vectorRange]
]

// Bytevectors (6.9)

// N.B., bytevector operations are unimplemented because they are inherently effectful.

// Control features (6.10)

const procedurePrim: L.Prim = (_env, args, app) =>
  // N.B., once we add non-function primitives, this will need to change.
  Promise.resolve(Utils.checkArgsResult('procedure?', ['any'], 'boolean?', args, app).andThen(_ =>
    ok(L.valueIsProcedure(args[0]))))

const applyPrim: L.Prim = (env, args, app) =>
  Utils.checkArgsResult('apply', ['procedure?'], 'list?', args, app).asyncAndThen(_ =>
    evaluateExp(env, L.nlecall(L.nlevalue(args[0]), L.valueListToArray_(args[1]).map(L.nlevalue))))

const stringMapPrim: L.Prim = async (env, args, app) =>
  Utils.checkArgsResult('string-map', ['procedure?', 'string?'], 'string?', args, app).asyncAndThen(async _ => {
    const result = await Promise.all((args[1] as string).split('').map(async c =>
      await evaluateExp(env, L.nlecall(L.nlevalue(args[0]), [L.nlechar(c)]))))
    return join(result).andThen(vs => {
      for (const v of vs) {
        if (!L.valueIsChar(v)) {
          return runtimeError(msg('error-precondition-not-met', 'string-map', 1, 'produces a character', Pretty.expToString(0, L.nlevalue(v))), app)
        }
      }
      return ok(vs.map(v => L.valueGetChar_(v)).join(''))
    })
  })

/**
 * @param arr - a rectangular array of arrays, i.e., each array has the same
 * length
 * @returns the transposition of this array of arrays where rows become columns
 * and columns become rows.
 */
function transpose <T> (arr: T[][]): T[][] {
  if (arr.length === 0) { return [] }
  const numArrays = arr.length
  // N.B., assumed that all arrays have the same length
  const numArgs = arr[0].length
  const result: T[][] = []
  for (let i = 0; i < numArgs; i++) {
    result.push([])
  }
  for (let i = 0; i < numArgs; i++) {
    for (let j = 0; j < numArrays; j++) {
      result[i].push(arr[j][i])
    }
  }
  return result
}

const mapPrim: L.Prim = async (env, args, app) =>
  Utils.checkArgsResult('map', ['procedure?', 'list?'], 'list?', args, app).asyncAndThen(async _ => {
    const fn = args[0]
    // N.B., non-recursive, linear-time map when only one list is involved
    if (args.length === 2) {
      const list = args[1]
      if (list === null) {
        return Promise.resolve(ok(null))
      } else {
        let cur = list as L.PairType
        const ret = L.vpair(cur.fst, null) as L.PairType
        let result = await evaluateExp(env, L.nlecall(L.nlevalue(fn), [L.nlevalue(cur.fst)]))
        if (result.tag === 'error') {
          return Promise.resolve(result)
        } else {
          ret.fst = result.value
        }
        let curRet: L.PairType = ret
        while (cur.snd !== null) {
          cur = cur.snd as L.PairType
          const newNode = L.vpair(null, null) as L.PairType
          result = await evaluateExp(env, L.nlecall(L.nlevalue(fn), [L.nlevalue(cur.fst)]))
          if (result.tag === 'error') {
            return Promise.resolve(result)
          } else {
            newNode.fst = result.value
          }
          curRet.snd = newNode
          curRet = newNode
        }
        return Promise.resolve(ok(ret))
      }
    } else {
      const lists = args.slice(1).map(L.valueListToArray_)
      if (!(lists.map(l => l.length).every(n => n === lists[0].length))) {
        return runtimeError(msg('error-precondition-not-met', 'map', 2,
          'all lists have the same length', Pretty.expToString(0, app)), app)
      }
      const xs = transpose(lists)
      const exp = L.arrayToList(xs.map(vs => L.nlecall(L.nlevalue(fn), asValues(vs))))
      return evaluateExp(env, exp)
    }
  })

// Additional list pipeline functions from racket/base

const filterPrim: L.Prim = async (env, args, app) =>
  Utils.checkArgsResult('filter', ['procedure?', 'list?'], undefined, args, app).asyncAndThen(async _ => {
    const fn = args[0]
    const list = L.valueListToArray_(args[1])
    const result = []
    for (let i = 0; i < list.length; i++) {
      const e = list[i]
      // TODO: revisit me once I fix evaluteExp to return a value
      const res = await evaluateExp(env, L.nlecall(L.nlevalue(fn), [L.nlevalue(e)]))
      if (res.tag === 'error') {
        return res
      } else if (!L.valueIsBoolean(res.value)) {
        return runtimeError(msg('error-type-filter-bool', L.nlevalue(res.value)), L.nlevalue(args[0]))
      } else if (res.value as boolean) {
        result.push(e)
      }
    }
    return ok(L.valueArrayToList(result))
  })

const foldPrim: L.Prim = async (env, args, app) =>
  Utils.checkArgsResult('fold', ['procedure?', 'any', 'list?'], undefined, args, app).asyncAndThen(async _ => {
    const fn = args[0]
    let result = args[1]
    const list = L.valueListToArray_(args[2])
    for (let i = 0; i < list.length; i++) {
      const e = list[i]
      const res = await evaluateExp(env, L.nlecall(L.nlevalue(fn), asValues([result, e])))
      if (res.tag === 'error') {
        return res
      } else {
        result = res.value
      }
    }
    return ok(result)
  })

const reducePrim: L.Prim = async (env, args, app) =>
  Utils.checkArgsResult('reduce', ['procedure?', 'list?'], undefined, args, app).asyncAndThen(async _ => {
    const fn = args[0]
    const list = L.valueListToArray_(args[1])

    if (list.length === 0) {
      return runtimeError(msg('error-precondition-not-met', 'reduce', '2', 'list is non-empty', Pretty.expToString(0, L.nlevalue(args[1]))), app)
    } else {
      return evaluateExp(env,
        L.nlecall(L.nlevar('fold'), [L.nlevalue(fn), L.nlevalue(list[0]), L.nlevalue(L.valueArrayToList(list.slice(1)))]))
    }
  })

const foldRightPrim: L.Prim = async (env, args, app) =>
  Utils.checkArgsResult('fold-right', ['procedure?', 'any', 'list?'], undefined, args, app).asyncAndThen(async _ => {
    const fn = args[0]
    let result = args[1]
    // N.B., reverse the list because we process it in right-to-left order
    const list = L.valueListToArray_(args[2]).reverse()
    for (let i = 0; i < list.length; i++) {
      const e = list[i]
      const res = await evaluateExp(env, L.nlecall(L.nlevalue(fn), asValues([e, result])))
      if (res.tag === 'error') {
        return res
      } else {
        result = res.value
      }
    }
    return ok(result)
  })

const reduceRightPrim: L.Prim = async (env, args, app) =>
  Utils.checkArgsResult('reduce-right', ['procedure?', 'list?'], undefined, args, app).asyncAndThen(async _ => {
    const fn = args[0]
    const list = L.valueListToArray_(args[1])
    if (list.length === 0) {
      return runtimeError(msg('error-precondition-not-met', 'reduce-right', '2', 'list is non-empty', Pretty.expToString(0, L.nlevalue(args[1]))), app)
    } else {
      return evaluateExp(env,
        L.nlecall(L.nlevar('fold-right'), asValues([
          fn,
          list[list.length - 1],
          L.valueArrayToList(list.slice(0, list.length - 1))])))
    }
  })

const vectorMapPrim: L.Prim = async (env, args, app) =>
  Utils.checkArgsResult('vector-map', ['procedure?', 'vector?'], 'vector?', args, app).asyncAndThen(async _ => {
    const fn = args[0]
    const vectors = args.slice(1) as L.Value[][]
    const len = vectors[0].length
    if (!(vectors.every(v => v.length === len))) {
      return runtimeError(msg('error-precondition-not-met', 'vector-map', 2,
        'all vectors have the same length', Pretty.expToString(0, app)), app)
    }
    const result: L.Value[] = []
    for (let i = 0; i < len; i++) {
      const args: L.Exp[] = new Array<L.Exp>(vectors.length)
      for (let j = 0; j < vectors.length; j++) {
        args[j] = L.nlevalue(vectors[j][i])
      }
      const v = await evaluateExp(env, L.nlecall(L.nlevalue(fn), args))
      if (v.tag === 'error') {
        return v
      } else {
        result.push(v.value)
      }
    }
    return ok(result)
  })

const vectorForEachPrim: L.Prim = async (env, args, app) =>
  Utils.checkArgsResult('vector-for-each', ['procedure?', 'vector?'], undefined, args, app).asyncAndThen(async _ => {
    const fn = args[0]
    const vectors = args.slice(1) as L.Value[][]
    const len = vectors[0].length
    if (!(vectors.every(v => v.length === len))) {
      return runtimeError(msg('error-precondition-not-met', 'vector-for-each', 2,
        'all vectors have the same length', Pretty.expToString(0, app)), app)
    }
    for (let i = 0; i < len; i++) {
      const args: L.Exp[] = new Array<L.Exp>(vectors.length + 1)
      args[0] = L.nlevalue(i)
      for (let j = 1; j < vectors.length + 1; j++) {
        args[j] = L.nlevalue(vectors[j - 1][i])
      }
      const v = await evaluateExp(env, L.nlecall(L.nlevalue(fn), args))
      if (v.tag === 'error') {
        return v
      }
    }
    return ok(undefined)
  })

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

// Additional control features

const vectorFilterPrim: L.Prim = (env, args, app) =>
  Utils.checkArgsResult('vector-filter', ['procedure?', 'vector?'], undefined, args, app).asyncAndThen(async _ => {
    const fn = args[0]
    const vector = args[1] as L.Value[]
    const result = []
    for (let i = 0; i < vector.length; i++) {
      const v = await evaluateExp(env, L.nlecall(L.nlevalue(fn), [L.nlevalue(vector[i])]))
      if (v.tag === 'error') {
        return v
      } else if (!L.valueIsBoolean(v.value)) {
        return runtimeError(msg('error-type-filter-bool', Pretty.valueToString(0, v.value)), app)
      } else if (v.value as boolean) {
        result.push(vector[i])
      }
    }
    return ok(result)
  })

// TODO: implement fold/reduce variants for vectors

const voidQ: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('void?', ['any'], 'boolean', args, app).andThen(_ => ok(L.valueIsVoid(args[0]))))

const errorPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('error', ['string?'], 'any', args, app).andThen(_ =>
    runtimeError(msg('error-runtime', args[0], args.slice(1).map(v => Pretty.valueToString(0, v))), app)))

const qqPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('??', [], undefined, args, app).andThen(_ =>
    runtimeError(msg('error-hole', '??'), app)))

const composePrim: L.Prim = (env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('compose', ['procedure?'], 'procedure?', args, app).andThen(_ => {
    // N.B., process args in reverse order because the last function is the first to go!
    args = [...args].reverse()
    let composition = L.nlecall(L.nlevalue(args[0]), [L.nlevar('x')])
    for (let i = 1; i < args.length; i++) {
      composition = L.nlecall(L.nlevalue(args[i]), [composition])
    }
    return ok(L.vlambda([L.name('x', noRange())], composition))
  }))

const pipePrim: L.Prim = async (env, args, app) =>
  Utils.checkArgsResult('pipe', ['any', 'procedure?'], 'procedure?', args, app).asyncAndThen(async _ => {
    // N.B., process args in left-to-right order; the first function goes first!
    const x = args[0]
    let pipeline: L.Exp = L.nlevalue(x)
    for (let i = 1; i < args.length; i++) {
      pipeline = L.nlecall(L.nlevalue(args[i]), [pipeline])
    }
    return evaluateExp(env, pipeline)
  })

const rangePrim: L.Prim = (env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('range', [], 'number?', args, app).andThen(_ => {
    if (args.length === 0 || args.length > 3) {
      return runtimeError(msg('error-arity', 'range', '1--3', args.length), app)
    } else {
      const m = args.length === 1 ? 0 : args[0] as number
      const n = args.length === 1 ? args[0] as number : args[1] as number
      const step = args.length < 3 ? 1 : args[2] as number
      const arr = []
      // N.B., to prevent the internal infinite loop that would result
      // from having a zero step.
      if (step === 0) {
        return runtimeError(msg('error-precondition-not-met', 'range', '3', 'non-zero', step), app)
      }
      for (let i = m; step > 0 ? i < n : i > n; i += step) {
        arr.push(i)
      }
      return ok(L.valueArrayToList(arr))
    }
  }))

const randomPrim: L.Prim = async (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('random', ['integer?'], undefined, args, app).andThen(_ => {
    const n = args[0] as number
    return n <= 0
      ? runtimeError(msg('error-precondition-not-met', 'random', '1', 'positive integer', n), app)
      : ok(Math.floor(Math.random() * n))
  }))

const withHandlerPrim: L.Prim = async (env, args, app) =>
  Utils.checkArgsResult('with-handler', ['procedure?', 'procedure?'], 'any', args, app).asyncAndThen(async _ => {
    const handler = args[0] as L.FunctionType
    const fn = args[1] as L.FunctionType
    const fnArgs = args.slice(2)
    const result = await evaluateExp(env, L.nlecall(L.nlevalue(fn), fnArgs.map(v => L.nlevalue(v))))
    if (result.tag === 'error') {
      // N.B., somewhere a space is being introduced... need to get rid of it somewhere else!
      const err = result.details[0].message.trim()
      return await evaluateExp(env, L.nlecall(L.nlevalue(handler), [L.nlevalue(err as L.Value)]))
    } else {
      return ok(result.value)
    }
  })

const controlPrimitives: [string, L.Prim, L.Doc][] = [
  ['procedure?', procedurePrim, Docs.procedure],
  ['apply', applyPrim, Docs.apply],
  ['string-map', stringMapPrim, Docs.stringMap],
  ['map', mapPrim, Docs.map],
  ['filter', filterPrim, Docs.filter],
  ['fold', foldPrim, Docs.fold],
  ['reduce', reducePrim, Docs.reduce],
  ['fold-right', foldRightPrim, Docs.foldRight],
  ['reduce-right', reduceRightPrim, Docs.reduceRight],
  ['vector-map', vectorMapPrim, Docs.vectorMap],
  ['vector-for-each', vectorForEachPrim, Docs.vectorForEach],
  ['vector-filter', vectorFilterPrim, Docs.vectorFilter],
  ['vector-append', vectorAppendPrim, Docs.vectorAppend],
  ['voidQ', voidQ, Docs.voidQ],
  ['error', errorPrim, Docs.error],
  ['??', qqPrim, Docs.qq],
  ['compose', composePrim, Docs.compose],
  ['o', composePrim, Docs.o],
  ['|>', pipePrim, Docs.pipe],
  ['range', rangePrim, Docs.range],
  ['random', randomPrim, Docs.random],
  ['with-handler', withHandlerPrim, Docs.withHandler]
]

// Exceptions (6.11)

// N.B., exception operations are unimplemented because they are inherently effectful.

// Environments and Evaluation (6.12)

// N.B., platform-specific stuff with no need to be implemented.

// Input andoutput (6.13)

// N.B., in-browser, so can't implement directly without some level of virtualization.

// System interface (6.14)

// N.B., not implemented, all operating system-specific stuff.

// Additional constants

const elseConst: L.EnvEntry = L.entry(true, 'prelude', undefined, Docs.elseV)

const piConst: L.EnvEntry = L.entry(Math.PI, 'prelude', undefined, Docs.pi)

const voidConst: L.EnvEntry = L.entry(undefined, 'prelude', undefined, Docs.voidV)

export const preludeEnv = new L.Env([
  ...equivalencePrimitives,
  ...numericPrimitives,
  ...booleanPrimitives,
  ...pairListPrimitives,
  ...listPrimitives,
  ...stringPrimitives,
  ...vectorPrimitives,
  ...controlPrimitives
].map(v => [v[0], L.entry(L.vprim(v[1]), 'prelude', undefined, v[2])]))
  .append('else', elseConst)
  .append('pi', piConst)
  .append('π', piConst)
  .append('void', voidConst)
