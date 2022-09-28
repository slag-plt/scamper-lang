import * as L from '../lang.js'
import { join, ICE, ok, Result } from '../result.js'
import { evaluateExp, runtimeError } from '../runtime.js'
import * as Utils from './utils.js'
import { msg } from '../messages.js'
import * as Docs from './docs.js'
import * as Pretty from '../pretty.js'

function asNumbers (args: L.Exp[]): Result<number[]> {
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

const preludeEntry = (prim: L.Prim, docs?: L.Doc) => L.entry(L.nleprim(prim), 'prelude', undefined, docs)

// Equivalence predicates (6.1)

// N.B., don't need these functions:
//   (eqv? x y)
//   (eq? x y)
// Since we don't have effects beside vectors. Therefore, value vs. reference
// equality is not an issue!

const equalPrim: L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('equal?', ['any', 'any'], undefined, args, app).andThen(_ =>
    ok(L.nlebool(L.expEquals(args[0], args[1]))))

const equivalencePrimitives: [string, L.Prim, L.Doc | undefined][] = [
  ['equal?', equalPrim, Docs.equal]
]

// Numbers (6.2)

const numberPrim: L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('number?', ['any'], undefined, args, app).andThen(_ =>
    ok(L.nlebool(L.isNumber(args[0]))))

const realPrim: L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('real?', ['any'], undefined, args, app).andThen(_ =>
    ok(L.nlebool(L.isReal(args[0]))))

const integerPrim: L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('integer?', ['any'], undefined, args, app).andThen(_ =>
    ok(L.nlebool(L.isInteger(args[0]))))

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
  Utils.checkArgsResult('nan?', ['any'], undefined, args, app).andThen(_ =>
    ok(L.nlebool(Number.isNaN(L.asNum_(args[0])))))

function compareOp (symbol: string, op: (x: number, y: number) => boolean, args: L.Exp[], app: L.Exp): Result<L.Exp> {
  return Utils.checkArgsResult(symbol, ['number?', 'number?'], undefined, args, app).andThen(_ =>
    asNumbers(args).andThen(
      vs => ok(L.nlebool(op(vs[0], vs[1]))))
  )
}

const ltPrim: L.Prim = (_env, args, app) => compareOp('<', (x, y) => x < y, args, app)
const leqPrim : L.Prim = (_env, args, app) => compareOp('<=', (x, y) => x <= y, args, app)
const gtPrim : L.Prim = (_env, args, app) => compareOp('>', (x, y) => x > y, args, app)
const geqPrim : L.Prim = (_env, args, app) => compareOp('>=', (x, y) => x >= y, args, app)
const numeqPrim : L.Prim = (_env, args, app) => compareOp('=', (x, y) => x === y, args, app)

const zeroPrim : L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('zero?', ['number?'], undefined, args, app).andThen(_ =>
    ok(L.nlebool(L.asNum_(args[0]) === 0)))

const positivePrim : L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('positive?', ['number?'], undefined, args, app).andThen(_ =>
    ok(L.nlebool(L.asNum_(args[0]) > 0)))

const negativePrim : L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('negative?', ['number?'], undefined, args, app).andThen(_ =>
    ok(L.nlebool(L.asNum_(args[0]) < 0)))

const oddPrim : L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('odd?', ['number?'], undefined, args, app).andThen(_ =>
    ok(L.nlebool((L.asNum_(args[0]) & 1) === 1)))

const evenPrim : L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('even?', ['number?'], undefined, args, app).andThen(_ =>
    ok(L.nlebool((L.asNum_(args[0]) & 1) !== 1)))

function numericUOp (symbol: string, op: (x: number) => number, args: L.Exp[], app: L.Exp): Result<L.Exp> {
  return Utils.checkArgsResult(symbol, ['number?'], undefined, args, app).andThen(_ =>
    asNumbers(args).andThen(vs => ok(L.nlenumber(op(vs[0])))))
}

function numericBOp (symbol: string, op: (x: number, y: number) => number, args: L.Exp[], app: L.Exp): Result<L.Exp> {
  return Utils.checkArgsResult(symbol, ['number?', 'number?'], undefined, args, app).andThen(_ =>
    asNumbers(args).andThen(vs => ok(L.nlenumber(op(vs[0], vs[1])))))
}

function numericNOp (symbol: string, op: (x: number, y: number) => number, def: (x: number) => number, args: L.Exp[], app: L.Exp): Result<L.Exp> {
  return Utils.checkArgsResult(symbol, ['number?'], 'number?', args, app).andThen(_ =>
    args.length === 1
      ? ok(L.nlenumber(def(L.asNum_(args[0]))))
      : asNumbers(args).andThen(vs => ok(L.nlenumber(vs.reduce(op))))
)}

const maxPrim: L.Prim = (_env, args, app) => numericNOp('max', (x, y) => Math.max(x, y), x => x, args, app)
const minPrim: L.Prim = (_env, args, app) => numericNOp('min', (x, y) => Math.min(x, y), x => x, args, app)

const plusPrim: L.Prim = (_env, args, app) => numericNOp('+', (x, y) => x + y, x => x, args, app)
const minusPrim: L.Prim = (_env, args, app) => numericNOp('-', (x, y) => x - y, x => -x, args, app)
const timesPrim: L.Prim = (_env, args, app) => numericNOp('*', (x, y) => x * y, x => x, args, app)
const divPrim: L.Prim = (_env, args, app) => numericNOp('/', (x, y) => x / y, x => 1/x, args, app)

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
  const argErr = Utils.checkArgsResult('number->string', ['number?'], undefined, args, app)
  const e = args[0]
  return ok(L.nlestr(L.asNum_(e).toString()))
}

// TODO: implement:
//   (string->number s)
//   (string->number s radix)

const stringNumberPrim: L.Prim = (_env, args, app) => {
  const argErr = Utils.checkArgs('string->number', ['string?'], undefined, args, app)
  if (argErr) { return argErr }
  const s = L.asString_(args[0])
  if (/^[+-]?\d+$/.test(s) ) {
    return ok(L.nlenumber(parseInt(s)))
  } else if (/^[+-]?(\d+|(\d*\.\d+)|(\d+\.\d*))([eE][+-]?\d+)?$/.test(s)) {
    return ok(L.nlenumber(parseFloat(s)))
  } else {
    return runtimeError(msg('error-runtime-parsing', 'string->number', L.expToString(args[0]), 'number'), app)
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

const equalsEpsPrim: L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('=-eps', ['number?'], undefined, args, app).andThen(_ => {
    return ok(L.nlelam(['x', 'y'], L.nlecall(L.nlevar('<='), [
      L.nlecall(L.nlevar('abs'), [
        L.nlecall(L.nlevar('-'), [
          L.nlevar('x'), L.nlevar('y')
        ])
      ]),
      args[0]])))
  })

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
  Utils.checkArgsResult('not', ['boolean?'], 'boolean?', args, app).andThen(_ =>
    ok(L.nlebool(!L.asBool_(args[0]))))

const booleanPrim: L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('boolean?', ['any'], 'boolean?', args, app).andThen(_ =>
    ok(L.nlebool(L.isBoolean(args[0]))))

// From racket/base

const nandPrim: L.Prim = (env, args, app) =>
  Utils.checkArgsResult('nand', [], 'boolean?', args, app).andThen(_ =>
    evaluateExp(env, L.nlecall(L.nlevar('not'), [L.nleand(args)])))

const norPrim: L.Prim = (env, args, app) =>
  Utils.checkArgsResult('nand', [], 'boolean?', args, app).andThen(_ =>
    evaluateExp(env, L.nlecall(L.nlevar('not'), [L.nleor(args)])))

const impliesPrim: L.Prim = (env, args, app) =>
  Utils.checkArgsResult('implies', ['boolean?', 'boolean?'], undefined, args, app).andThen(_ =>
    evaluateExp(env, L.nleif(args[0], args[1], L.nlebool(true))))
   
const xorPrim: L.Prim = (env, args, app) =>
  Utils.checkArgsResult('xor', ['boolean?', 'boolean?'], undefined, args, app).andThen(_ =>    
    evaluateExp(env, L.nleor([
      L.nleand([args[0], L.nlecall(L.nlevar('not'), [args[1]])]),
      L.nleand([L.nlecall(L.nlevar('not'), [args[0]]), args[1]]),
    ])))

const booleanPrimitives: [string, L.Prim, L.Doc | undefined][] = [
  ['not', notPrim, Docs.not],
  ['boolean?', booleanPrim, Docs.boolean],
  ['nand', nandPrim, Docs.nand],
  ['nor', norPrim, Docs.nor],
  ['implies', impliesPrim, Docs.implies],
  ['xor', xorPrim, Docs.xor],
]

// Pairs and Lists (6.4)

const pairQPrim: L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('pair?', ['any'], 'boolean?', args, app).andThen(_ =>
    ok(L.nlebool(L.isPair(args[0]))))

const consPrim: L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('cons', ['any', 'any'], 'pair?', args, app).andThen(_ =>
    ok(L.epair(app.range, args[0], args[1])))

const pairPrim: L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('pair', ['any', 'any'], 'pair?', args, app).andThen(_ =>
    ok(L.epair(app.range, args[0], args[1])))

const carPrim : L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('car', ['pair?'], 'any', args, app).andThen(_ =>
    ok((args[0] as L.EPair).e1))

const cdrPrim : L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('cdr', ['pair?'], 'any', args, app).andThen(_ =>
    ok((args[0] as L.EPair).e2))

// N.B., set-car! and set-cdr! are unimplemented since we only implement the
// pure, functional subset of Scheme.

// TODO: implement caar, cadr, cdar, cddr, caaar, ..., cdddr in some elegant way

const nullPrim : L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('null?', ['any'], 'boolean?', args, app).andThen(_ =>
    ok(L.nlebool(args[0].tag === 'nil')))

const listQPrim : L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('list?', ['any'], 'boolean?', args, app).andThen(_ => 
    ok(L.nlebool(L.isList(args[0]))))

const pairListPrimitives: [string, L.Prim, L.Doc | undefined][] = [
  ['pair?', pairQPrim, Docs.pairQ],
  ['cons', consPrim, Docs.cons],
  ['pair', pairPrim, Docs.pair],
  ['car', carPrim, Docs.car],
  ['cdr', cdrPrim, Docs.cdr],
  ['null?', nullPrim, Docs.nullQ],
  ['list?', listQPrim, Docs.listQ]
]

const listPrim: L.Prim = function (_env, args, app) {
  return ok(L.arrayToList(args))
}

const makeListPrim: L.Prim = function (_env, args, app) {
  // N.B., (make-list k) returns the empty list, but this behavior is weird, so we don't replicate it!
  const argErr = Utils.checkArgs('make-list', ['number?', 'any'], undefined, args, app)
  if (argErr) { return argErr }
  const n = L.asNum_(args[0])
  const fill = args[1]
  let ret: L.Exp = L.nlenil()
  for (let i = 0; i < n; i++) {
    ret = L.nlepair(fill, ret)
  }
  return ok(ret)
}

const lengthPrim: L.Prim = function (_env, args, app) {
  const argErr = Utils.checkArgs('length', ['list?'], undefined, args, app)
  if (argErr) { return argErr }
  let length = 0
  let e: L.Exp = args[0]
  while (e.tag !== 'nil') {
    if (e.tag === 'pair') {
      length += 1
      e = e.e2
    } else {
      throw new ICE('lengthPrim', `Processing a non-list that we thought was a list: ${L.expToString(app)}`)
    }
  }
  return ok(L.nlenumber(length))
}

function appendOne_ (l1: L.Exp, l2: L.Exp): L.Exp {
  if (l1.tag === 'nil') {
    return l2
  } else if (l1.tag === 'pair') {
    return L.nlepair(l1.e1, appendOne_(l1.e2, l2))
  } else {
    throw new ICE('appendOne', `Non-list passed: ${L.expToString(l1)}`)
  }
}

const appendPrim: L.Prim = function (_env, args, app) {
  const argErr = Utils.checkArgs('append', ['list?'], 'list?', args, app)
  let ret = args[0]
  for (let i = 1; i < args.length; i++) {
    ret = appendOne_(ret, args[i])
  }
  return ok(ret)
}

const reversePrim: L.Prim = function (_env, args, app) {
  const argErr = Utils.checkArgs('reverse', ['list?'], 'list?', args, app)
  if (argErr) { return argErr }
  const queue = []
  let e = args[0]
  while (e.tag !== 'nil') {
    queue.push(e)
    e = (e as L.EPair).e2
  }
  queue.reverse()
  let ret: L.Exp = L.nlenil()
  while (queue.length > 0) {
    const next = queue.pop() as L.EPair
    ret = L.nlepair(next.e1, ret)
  }
  return ok(ret)
}

const listTailPrim: L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('list-tail', ['list?', 'number?'], 'list?', args, app).andThen(_ => {
    const list = L.unsafeListToArray(args[0])
    const k = L.asNum_(args[1])
    if (k < 0 || k > list.length) {
      return runtimeError(msg('error-precondition-not-met', 'list-tail', 2, '<= length of list', k), app)
    }
    return ok(L.arrayToList(list.slice(k)))
  })

const listTakePrim: L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('list-take', ['list?', 'number?'], 'list?', args, app).andThen(_ => {
    const list = L.unsafeListToArray(args[0])
    const k = L.asNum_(args[1])
    if (k < 0 || k > list.length) {
      return runtimeError(msg('error-precondition-not-met', 'list-take', 2, '<= length of list', k), app)
    }
    return ok(L.arrayToList(list.slice(0, k)))
  })

const listRefPrim: L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('list-ref', ['list?', 'number?'], 'any', args, app).andThen(_ => {
    const list = L.unsafeListToArray(args[0])
    const i = L.asNum_(args[1])
    if (i < 0 || i >= list.length) {
      return runtimeError(msg('error-precondition-not-met', 'list-ref', 2, 'valid index into list', i), app)
    }
    return ok(list[i])
  })

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

// Other list functions

const indexOfPrim: L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('index-of', ['list?', 'any'], 'number?', args, app).andThen(_ => {
    const list = L.unsafeListToArray(args[0])
    for (let i = 0; i < list.length; i++) {
      if (L.expEquals(list[i], args[1])) {
        return ok(L.nlenumber(i))
      }
    }
    return ok(L.nlenumber(-1))
  })

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
  ['index-of', indexOfPrim, Docs.indexOf]
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
  Utils.checkArgsResult('char?', ['any'], undefined, args, app).andThen(_ =>
    ok(L.nlebool(L.isChar(args[0]))))

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
    if (err) { return err }
    return ok(L.nlebool(pairwiseSatisfies((a, b) => f(L.asChar_(a), L.asChar_(b)), args)))
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
    if (err) { return err }
    return ok(L.nlebool(f(L.asChar_(args[0]))))
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
  if (err) { return err }
  const char = L.asChar_(args[0])
  const n = parseInt(char, 10)
  if (isNaN(n)) {
    return runtimeError(msg('error-precondition-not-met', 'digit-value', 'decimal digit', char), app)
  } else {
    return ok(L.nlenumber(n))
  }
}

const charIntegerPrim: L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('char->integer', ['char?'], undefined, args, app).andThen(_ =>
    ok(L.nlenumber(L.asChar_(args[0]).codePointAt(0)!)))

const integerCharPrim: L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('integer->char', ['integer?'], undefined, args, app).andThen(_ =>
    ok(L.nlechar(String.fromCodePoint(L.asNum_(args[0])))))

const charUpcasePrim: L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('char-upcase', ['char?'], undefined, args, app).andThen(_ =>
    ok(L.nlechar(L.asChar_(args[0]).toUpperCase())))

const charDowncasePrim: L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('char-downcase', ['char?'], undefined, args, app).andThen(_ =>
    ok(L.nlechar(L.asChar_(args[0]).toLowerCase())))

// N.B., "folding" in Unicode returns a character to a "canonical" form, suitable for
// comparison in a "case-insensitive" manner. toLowerCase is Unicode aware, so maybe
// this implementation works. But... yea, maybe not!
//
// See: https://unicode.org/reports/tr18/#General_Category_Property
const charFoldcasePrim: L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('char-foldcase', ['char?'], 'char?', args, app).andThen(_ =>
    ok(L.nlechar(L.asChar_(args[0]).toLowerCase())))

// Strings (6.7)

const stringQPrim: L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('string?', ['any'], 'boolean?', args, app).andThen(_ =>
    ok(L.nlebool(L.isString(args[0]))))

// N.B., we don't implement the (make-string k) variant because our strings are
// immutable, so having an "empty" string of size k does not make sense.
const makeStringPrim: L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('make-string', ['integer?', 'char?'], 'string?', args, app).andThen(_ =>
    ok(L.nlestr(L.asChar_(args[1]).repeat(L.asNum_(args[0])))))

const stringPrim: L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('string', ['char?'], 'char?', args, app).andThen(_ =>
    ok(L.nlestr(args.map((e) => L.asChar_(e)).join(''))))

const stringLengthPrim: L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('string-length', ['string?'], undefined, args, app).andThen(_ =>
    ok(L.nlenumber(L.asString_(args[0]).length)))

const stringRefPrim: L.Prim = function (_env, args, app) {
  const argErr = Utils.checkArgs('string-ref', ['string?', 'integer?'], undefined, args, app)
  if (argErr) { return argErr }
  const str = L.asString_(args[0])
  const i = L.asNum_(args[1])
  if (i >= 0 && i < str.length) {
    return ok(L.nlechar(str[i]))
  } else {
    return runtimeError(msg('error-index-string', i, str), app)
  }
}

// N.B., string-set! is unimplemented since it is effectful.

function mkStringComparePrim (name: string, f: (a: string, b: string) => boolean): L.Prim {
  return (_env, args, app) =>
    Utils.checkArgsResult(name, [], 'string?', args, app).andThen(_ =>
      ok(L.nlebool(pairwiseSatisfies((a, b) =>
        f(L.asString_(a), L.asString_(b)), args))))
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
  Utils.checkArgsResult('string-upcase', ['string?'], undefined, args, app).andThen(_ =>
    ok(L.nlestr(L.asString_(args[0]).toUpperCase())))

const stringDowncasePrim: L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('string-downcase', ['string?'], undefined, args, app).andThen(_ =>
    ok(L.nlestr(L.asString_(args[0]).toLowerCase())))

const stringFoldcasePrim: L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('string-foldcase', ['string?'], undefined, args, app).andThen(_ =>
    ok(L.nlestr(L.asString_(args[0]).toLowerCase())))

const substringPrim: L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('substring', ['string?', 'integer?', 'integer?'], undefined, args, app).andThen(_ =>
    ok(L.nlestr(L.asString_(args[0]).substring(L.asNum_(args[1]), L.asNum_(args[2])))))

const stringAppendPrim: L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('string-append', ['string?'], 'string?', args, app).andThen(_ =>
    ok(L.nlestr(args.map(L.asString_).join(''))))

const stringListPrim: L.Prim = (_env, args, app) => {
  if (args.length !== 1 && args.length !== 3) {
    return runtimeError(msg('error-arity', 'string->list', '1 or 3', args.length), app)
  }
  if (!L.isString(args[0])) {
    return runtimeError(msg('error-type-expected-fun', 1, 'string->list', 'string', args[0].tag), app)
  }
  const str = L.asString_(args[0])
  let start, end
  if (args.length === 1) {
    start = 0
    end = str.length
  } else {
    if (!L.isInteger(args[1])) {
      return runtimeError(msg('error-type-expected-fun', 2, 'string->list', 'integer', args[1].tag), app)
    }
    if (!L.isInteger(args[2])) {
      return runtimeError(msg('error-type-expected-fun', 3, 'string->list', 'integer', args[2].tag), app)
    }
    start = L.asNum_(args[1])
    end = L.asNum_(args[2])
  }
  return ok(L.arrayToList(
    str.substring(start, end).split('').map(L.nlechar)))
}

const listStringPrim: L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('list->string', ['list?'], undefined, args, app).andThen(_ => {
    const lst = L.asList_(args[0])
    for (const e of lst) {
      if (!L.isChar(e)) {
        return runtimeError(msg('error-type-expected-fun',
          'list->string', 1, 'list of chars', e), app)
      }
    }
    return ok(L.nlestr(L.asList_(args[0]).map(L.asChar_).join('')))
  })

// N.B., the following functions:
//
//   (string-copy string)
//   (string-copy string start)
//   (string-copy string start end)
//
// and string-copy! and string-fill! are unimplemented since they don't make
// sense in an immutable context.

// Additional functions from racket/string.

const stringSplitPrim: L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('string-split', ['string?', 'string?'], undefined, args, app).andThen(_ =>
    ok(L.arrayToList(L.asString_(args[0]).split(L.asString_(args[1])).map(L.nlestr))))

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
  ['string-split', stringSplitPrim, Docs.stringSplit],
  ['string-append', stringAppendPrim, Docs.stringAppend]
]

// Vectors (6.8)

// N.B., vector operations are unimplemented because they are inherently effectful.

// Bytevectors (6.9)

// N.B., bytevector operations are unimplemented because they are inherently effectful.

// Control features (6.10)

const procedurePrim: L.Prim = (_env, args, app) =>
  // N.B., once we add non-function primitives, this will need to change.
  Utils.checkArgsResult('procedure?', ['any'], 'boolean?', args, app).andThen(_ =>
    ok(L.nlebool(L.isProcedure(args[0]))))

const applyPrim: L.Prim = (env, args, app) =>
  Utils.checkArgsResult('apply', ['procedure?'], 'list?', args, app).andThen(_ =>
    evaluateExp(env, L.nlecall(args[0], L.unsafeListToArray(args[1]))))

const stringMapPrim: L.Prim = (env, args, app) =>
  Utils.checkArgsResult('string-map', ['procedure?', 'string?'], 'string?', args, app).andThen(_ =>
    join(L.asString_(args[1]).split('').map(c =>
      evaluateExp(env, L.nlecall(args[0], [L.nlechar(c)])))).andThen(vs => {
        for (const v of vs) {
          if (!L.isChar(v)) {
            return runtimeError(msg('error-precondition-not-met', 'string-map', 1, 'produces a character', Pretty.expToString(0, v)), app)
          }
        }
        return ok(L.nlestr(vs.map(v => L.asChar_(v)).join('')))
      }))

/*
[ [ 1,  2, 3 ]
, [ 4,  5, 6 ]
, [ 7,  8, 9 ]
, [10, 11, 12]
]


*/

/**
 * @param arr - a rectangular array of arrays, i.e., each array has the same
 * length
 * @returns the transposition of this array of arrays where rows become columns
 * and columns become rows.
 */
function transpose <T>(arr: T[][]): T[][] {
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

const mapPrim: L.Prim = (env, args, app) =>
  Utils.checkArgsResult('map', ['procedure?', 'list?'], 'list?', args, app).andThen(_ => {
    const fn = args[0]
    const lists = args.slice(1).map(L.unsafeListToArray)
    if (!(lists.map(l => l.length).every(n => n === lists[0].length))) {
      return runtimeError(msg('error-precondition-not-met', 'map', 2,
        'all lists have the same length', Pretty.expToString(0, app)), app)
    }
    const xs = transpose(lists)
    return evaluateExp(env, L.arrayToList(xs.map(vs => L.nlecall(fn, vs))))
  })

// Additional list pipeline functions from racket/base

const filterPrim: L.Prim = (env, args, app) =>
  Utils.checkArgsResult('filter', ['procedure?', 'list?'], undefined, args, app).andThen(_ => {
    const fn = args[0]
    const list = L.asList_(args[1])
    const result = []
    for (let i = 0; i < list.length; i++) {
      const e = list[i]
      const res = evaluateExp(env, L.nlecall(fn, [e]))
      if (res.tag === 'error') {
        return res
      } else if (!L.isBoolean(res.value)) {
        return runtimeError(msg('error-type-filter-bool', res.value.tag), args[0])
      } else if (L.asBool_(res.value)) {
        result.push(e)
      }
    }
    return ok(L.arrayToList(result))
  })

const foldPrim: L.Prim = (env, args, app) =>
  Utils.checkArgsResult('fold', ['procedure?', 'any', 'list?'], undefined, args, app).andThen(_ => {
    const fn = args[0]
    let result = args[1]
    const list = L.asList_(args[2])
    for (let i = 0; i < list.length; i++) {
      const e = list[i]
      const res = evaluateExp(env, L.nlecall(fn, [result, e]))
      if (res.tag === 'error') {
        return res
      } else {
        result = res.value
      }
    }
    return ok(result)
  })

const reducePrim: L.Prim = (env, args, app) =>
  Utils.checkArgsResult('reduce', ['procedure?', 'list?'], undefined, args, app).andThen(_ => {
    const fn = args[0]
    const list = L.asList_(args[1])

    if (list.length === 0) {
      return runtimeError(msg('error-precondition-not-met', 'reduce', '2', 'list is non-empty', L.expToString(args[1])), app)
    } else {
      return evaluateExp(env,
        L.nlecall(L.nlevar('fold'), [args[0], list[0], L.arrayToList(list.slice(1))]))
    }
  })

const foldRightPrim: L.Prim = (env, args, app) =>
  Utils.checkArgsResult('fold-right', ['procedure?', 'any', 'list?'], undefined, args, app).andThen(_ => {
    const fn = args[0]
    let result = args[1]
    // N.B., reverse the list because we process it in right-to-left order
    const list = L.asList_(args[2]).reverse()
    for (let i = 0; i < list.length; i++) {
      const e = list[i]
      const res = evaluateExp(env, L.nlecall(fn, [e, result]))
      if (res.tag === 'error') {
        return res
      } else {
        result = res.value
      }
    }
    return ok(result)
  })

const reduceRightPrim: L.Prim = (env, args, app) =>
  Utils.checkArgsResult('reduce-right', ['procedure?', 'list?'], undefined, args, app).andThen(_ => {
    const fn = args[0]
    const list = L.asList_(args[1])

    if (list.length === 0) {
      return runtimeError(msg('error-precondition-not-met', 'reduce-right', '2', 'list is non-empty', L.expToString(args[1])), app)
    } else {
      return evaluateExp(env,
        L.nlecall(L.nlevar('fold-right'), [args[0], list[list.length - 1], L.arrayToList(list.slice(0, list.length -1))]))
    }
  })

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

// Additional control features
const errorPrim: L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('error', ['string?'], undefined, args, app).andThen(_ =>
    runtimeError(msg('error-runtime', L.asString_(args[0])), app))

const qqPrim: L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('??', [], undefined, args, app).andThen(_ =>
    runtimeError(msg('error-hole', '??'), app))

const composePrim: L.Prim = (env, args, app) =>
  Utils.checkArgsResult('compose', ['procedure?'], 'procedure?', args, app).andThen(_ => {
    // N.B., process args in reverse order because the last function is the first to go!
    args = [...args].reverse()
    return ok(L.nlelam(['x'], args.slice(1).reduce((e, f) => L.nlecall(f, [e]), L.nlecall(args[0], [L.nlevar('x')]))))
  })

const pipePrim: L.Prim = (env, args, app) =>
  Utils.checkArgsResult('pipe', ['any', 'procedure?'], 'procedure?', args, app).andThen(_ => {
    // N.B., process args in left-to-right order; the first function goes first!
    const x = args[0]
    const f1 = args[1]
    const fs = args.slice(2)
    return evaluateExp(env, fs.reduce((e, f) => L.nlecall(f, [e]), L.nlecall(f1, [x])))
  })

const rangePrim: L.Prim = (env, args, app) =>
  Utils.checkArgsResult('range', [], 'integer?', args, app).andThen(_ => {
    if (args.length == 0 || args.length > 3) {
      return runtimeError(msg('error-arity', 'range', '1--3', args.length), app)
    } else {
      let m = args.length == 1 ? 0 : L.asNum_(args[0])
      let n = args.length == 1 ? L.asNum_(args[0]) : L.asNum_(args[1])
      let step = args.length < 3 ? 1 : L.asNum_(args[2])
      const arr = []
      // N.B., to prevent the internal infinite loop that would result
      // from having a zero step.
      if (step == 0) {
        return runtimeError(msg('error-precondition-not-met', 'range', '3', 'non-zero', step), app)
      }
      for (let i = m; step > 0 ? i < n : i > n; i += step) {
        arr.push(i)
      }
      return ok(L.arrayToList(arr.map(n => L.nlenumber(n))))
    }
  })

const controlPrimitives: [string, L.Prim, L.Doc | undefined][] = [
  ['procedure?', procedurePrim, Docs.procedure],
  ['apply', applyPrim, Docs.apply],
  ['string-map', stringMapPrim, Docs.stringMap],
  ['map', mapPrim, Docs.map],
  ['filter', filterPrim, Docs.filter],
  ['fold', foldPrim, Docs.fold],
  ['reduce', reducePrim, Docs.reduce],
  ['fold-right', foldRightPrim, Docs.foldRight],
  ['reduce-right', reduceRightPrim, Docs.reduceRight],
  ['error', errorPrim, Docs.error],
  ['??', qqPrim, Docs.qq],
  ['compose', composePrim, Docs.compose],
  ['o', composePrim, Docs.o],
  ['|>', pipePrim, Docs.pipe],
  ['range', rangePrim, Docs.range],
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

const elseConst: L.EnvEntry = L.entry(L.nlebool(true), 'prelude', undefined, Docs.elseV)

export const preludeEnv = new L.Env([
  ...equivalencePrimitives,
  ...numericPrimitives,
  ...booleanPrimitives,
  ...pairListPrimitives,
  ...listPrimitives,
  ...stringPrimitives,
  ...controlPrimitives
].map(v => [v[0], L.entry(L.nleprim(v[1]), 'prelude', undefined, v[2])]))
  .append('else', elseConst)
