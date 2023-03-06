/* eslint-disable no-use-before-define */

import { Range, noRange } from './loc.js'
import { ErrorDetails, ICE, Result } from './result.js'

// #region Names

export type BracketKind = '(' | '[' | '{'

type Name = { value: string, range: Range }
const name = (value: string, range: Range): Name => ({ value, range })
const nlname = (value: string): Name => name(value, noRange())

// #endregion

// #region Value forms

export type TaggedObject = FunctionType | CharType | PairType | StructType
export type LambdaType = { _scamperTag: 'lambda', args: Name[], body: Exp, env?: Env }
export type PrimType = { _scamperTag: 'prim', fn: Prim }
export type FunctionType = LambdaType | PrimType
export type CharType = { _scamperTag: 'char', value: string }
export type PairType = { _scamperTag: 'pair', fst: Value, snd: Value, isList: boolean }
export type StructType = { _scamperTag: 'struct', kind: string, fields: Value[] }

/** In Scamper, a Value is, directly, a Javascript value. */
export type Value = boolean | number | string | null | object | Value[] | TaggedObject | undefined

/*
 * Scamper-Javascript value conversion:
 *
 * (boolean? e) <=> typeof e === 'boolean'
 * (number? e) <=> typeof e === 'number'
 * (string? e) <==> typeof e === 'string'
 * (null? e) <==> e === null
 * (char? e) <==> typeof e === 'object': { _scamperTag: 'char', value: string }  <-- need to deprecate this!
 * (function? e) <==> typeof e === 'object': { _scamperTag: 'lambda', args: Name[], body: Exp } or { _scamperTag: 'prim', fn: Prim }
 * (pair? e) <==> typeof e === 'object': { _scamperTag: 'pair', fst: Value, snd: Value }
 * (struct? e) <==> typeof e === 'object': { _scamperTag: 'struct', 'kind': string, fields: Value[] }
 * (object? e) <==> typeof e === 'object': { ... } (no _scamperTag field)
 * (vector? e) <==> Array.isArray(e)
 * (void? e) <==> e === undefined
 *
 * TODO: do we need to "uniquify" the fields so that, e.g., an object can't
 * mock-up a struct?
 */

export const vchar = (value: string): Value => ({ _scamperTag: 'char', value })
export const vlambda = (args: Name[], body: Exp, env?: Env): Value => ({ _scamperTag: 'lambda', args, body, env })
export const vprim = (fn: Prim): Value => ({ _scamperTag: 'prim', fn })
export const vpair = (fst: Value, snd: Value): Value => ({
  _scamperTag: 'pair',
  fst,
  snd,
  isList: snd === null || (valueIsPair(snd) && (snd as PairType).isList)
})
export const vstruct = (kind: string, fields: Value[]): Value => ({ _scamperTag: 'struct', kind, fields })

export const valueIsAny = (v: Value): boolean => true
export const isTaggedObject = (v: Value): boolean =>
  typeof v === 'object' && v !== null && Object.hasOwn(v as object, '_scamperTag')
export const valueIsBoolean = (v: Value): boolean => typeof v === 'boolean'
export const valueIsNumber = (v: Value): boolean => typeof v === 'number'
export const valueIsInteger = (v: Value): boolean => valueIsNumber(v) && Number.isInteger(v)
export const valueIsReal = (v: Value): boolean => valueIsNumber(v) && !Number.isInteger(v)
export const valueIsString = (v: Value): boolean => typeof v === 'string'
export const valueIsNull = (v: Value): boolean => v === null
export const valueIsChar = (v: Value): boolean => isTaggedObject(v) && (v as TaggedObject)._scamperTag === 'char'
export const valueIsLambda = (v: Value): boolean => isTaggedObject(v) && (v as TaggedObject)._scamperTag === 'lambda'
export const valueIsPrim = (v: Value): boolean => isTaggedObject(v) && (v as TaggedObject)._scamperTag === 'prim'
export const valueIsProcedure = (v: Value): boolean => valueIsLambda(v) || valueIsPrim(v)
export const valueIsPair = (v: Value): boolean => isTaggedObject(v) && (v as TaggedObject)._scamperTag === 'pair'
export const valueIsStruct = (v: Value): boolean => isTaggedObject(v) && (v as TaggedObject)._scamperTag === 'struct'
export const valueIsStructKind = (v: Value, kind: string): boolean =>
  valueIsStruct(v) && (v as StructType).kind === kind
export const valueIsAnyStructKind = (v: Value, kinds: string[]): boolean =>
  valueIsStruct(v) && kinds.includes((v as StructType).kind)
export const valueIsObject = (v: Value): boolean => typeof v === 'object' && v !== null && !Object.hasOwn(v as object, '_scamperTag')
export const valueHasProperty = (v: Value, p: string): boolean => valueIsObject(v) && Object.hasOwn(v as object, p)
export const valueHasPropertyValue = (v: Value, p: string, x: any): boolean =>
  // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
  valueHasProperty(v, p) && (v as any)[p] === x
export const valueIsVector = (v: Value): boolean => Array.isArray(v)
export const valueIsVoid = (v: Value): boolean => v === undefined

export const valueGetChar_ = (v: Value): string => (v as CharType).value

export function valueIsList (v: Value): boolean {
  while (true) {
    if (v === null) {
      return true
    } else if (valueIsPair(v)) {
      v = (v as PairType).snd
    } else {
      return false
    }
  }
}

export function valueListToArray_ (v: Value): Value[] {
  const result: Value[] = []
  while (true) {
    if (v === null) {
      return result
    } else if (valueIsPair(v)) {
      result.push((v as PairType).fst)
      v = (v as PairType).snd
    } else {
      throw new ICE('valueListToArray', `valueListToArray: not a list: ${v ? v.toString() : 'void'}`)
    }
  }
}

export function valueArrayToList (vs: Value[]): Value {
  let result: Value = null
  // N.B., append in backwards order because of our cons representation for lists.
  for (let i = vs.length - 1; i >= 0; i--) {
    result = vpair(vs[i], result)
  }
  return result
}

export function valueToString (v: Value): string {
  if (typeof v === 'boolean') {
    return v ? '#t' : '#f'
  } else if (typeof v === 'number') {
    return v.toString()
  } else if (typeof v === 'string') {
    return `"${v}"`
  } else if (valueIsChar(v)) {
    return `#\\${(v as CharType).value}`
  } else if (valueIsLambda(v) || valueIsPrim(v)) {
    return '[object Function]'
  } else if (valueIsPair(v)) {
    return valueIsList(v)
      ? `(list ${valueListToArray_(v).map(valueToString).join(' ')})`
      : `(cons ${valueToString((v as PairType).fst)} ${valueToString((v as PairType).snd)})`
  } else if (valueIsStruct(v)) {
    return `(struct ${(v as StructType).kind.toString()} ${(v as StructType).fields.map(v => valueToString(v)).join(' ')})`
  } else if (Array.isArray(v)) {
    throw new ICE('valueToString', 'vector not yet implemented')
  } else if (v === null) {
    return 'null'
  } else if (typeof v === 'object') {
    // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
    if (Object.hasOwn(v, 'renderAs') && (v as any).renderAs === 'drawing') {
      return '[object Drawing]'
    // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
    } else if (Object.hasOwn(v, 'renderAs') && (v as any).renderAs === 'composition') {
      return '[object Composition]'
    } else {
      return '[object Object]'
    }
  }
  // eslint-disable-next-line @typescript-eslint/restrict-template-expressions
  throw new ICE('valueToString', `unknown value encountered: ${v}`)
}

export function valueEquals (v1: Value, v2: Value): boolean {
  if (valueIsBoolean(v1) && valueIsBoolean(v2)) {
    return v1 === v2
  } else if (valueIsNumber(v1) && valueIsNumber(v2)) {
    return v1 === v2
  } else if (valueIsString(v1) && valueIsString(v2)) {
    return v1 === v2
  } else if (valueIsNull(v1) && valueIsNull(v2)) {
    return true
  } else if (valueIsChar(v1) && valueIsChar(v2)) {
    return (v1 as CharType).value === (v2 as CharType).value
  } else if (valueIsProcedure(v1) && valueIsProcedure(v2)) {
    // N.B., strict memory equality for functions
    return v1 === v2
  } else if (valueIsPair(v1) && valueIsPair(v2)) {
    const p1 = v1 as PairType
    const p2 = v2 as PairType
    return valueEquals(p1.fst, p2.fst) && valueEquals(p1.snd, p2.snd)
  } else if (valueIsStruct(v1) && valueIsStruct(v2)) {
    const s1 = v1 as StructType
    const s2 = v2 as StructType
    return s1.kind === s2.kind && s1.fields.length === s2.fields.length &&
      s1.fields.every((v, i) => valueEquals(v, s2.fields[i]))
  } else if (valueIsObject(v1) && valueIsObject(v2)) {
    // TODO: should implement a deep equality check with Object methods
    return v1 === v2
  } else if (valueIsVector(v1) && valueIsVector(v2)) {
    const a1 = v1 as Value[]
    const a2 = v2 as Value[]
    return a1.length === a2.length && a1.every((v, i) => valueEquals(v, a2[i]))
  } else {
    return false
  }
}

// #endregion

// #region Expression forms

/**
 * A `Doc` is a convenience class for constructing docstrings for library
 * primitives.
 */
export class Doc {
  /**
   *
   * @param sig A docstring corresponding to the signature of the function.
   * @param args An array of docstrings for each of the function's arguments.
   * @param desc A prose description of the behavior of the function.
   */
  // eslint-disable-next-line no-useless-constructor
  constructor (public sig: string, public args: string[], public desc: string) { }

  /**
   * @returns A string containing the docstring formatted in Markdown.
   */
  public docToMarkdown (): string {
    return `
~~~
${this.sig.trim()}

${this.args.map(arg => '  ' + arg.trim()).join('\n')}
~~~

${this.desc.trim()}
  `.trim()
  }
}

export type EnvEntry = { value: Value, source: string, range?: Range, doc?: Doc }
export const entry = (value: Value, source: string, range?: Range, doc?: Doc): EnvEntry =>
  ({ value, source, range, doc })

export class Env {
  parent?: Env
  entries: Map<string, EnvEntry>

  constructor (entries?: Iterable<[string, EnvEntry]>, parent?: Env) {
    if (entries) {
      this.entries = new Map(entries)
    } else {
      this.entries = new Map()
    }
    this.parent = parent
  }

  public has (key: string): boolean {
    return this.entries.has(key) || (this.parent === undefined ? false : this.parent.has(key))
  }

  public get (key: string): EnvEntry | undefined {
    return this.entries.has(key)
      ? this.entries.get(key)
      : this.parent === undefined ? undefined : this.parent.get(key)
  }

  public items (): [string, EnvEntry][] {
    return [...this.entries.entries(), ...this.parent === undefined ? [] : this.parent.items()]
  }

  public names (): string[] {
    return [...this.entries.keys(), ...this.parent === undefined ? [] : this.parent.names()]
  }

  public append (key: string, value: EnvEntry): Env {
    return new Env([...this.entries, [key, value]], this.parent)
  }

  public concat (other: Env): Env {
    return new Env([...this.entries, ...other.entries], this.parent)
  }

  public without (keys: string[]): Env {
    const ret = new Env(this.items(), this.parent?.without(keys))
    keys.forEach(k => ret.entries.delete(k))
    return ret
  }

  public set (key: string, value: EnvEntry): void {
    this.entries.set(key, value)
  }

  public setAll (entries: Iterable<[string, EnvEntry]>): void {
    for (const [k, v] of entries) {
      this.set(k, v)
    }
  }
}

/**
 * The type of primitive function implementations.
 * @param env - the current execution environment.
 * @param args - the values passed to the primitive function.
 * @param app - the full application expression, for error-reporting purposes.
 * @returns the result of the primitive function.
 */
type Prim = (env: Env, args: Value[], app: Exp) => Promise<Result<Value>>

/** Literal expressions */
type Lit
  = LBool
  | LNum
  | LChar
  | LStr
  // | LSym     // Symbols
  // | LBVec    // Byte vector
  // | LQuote   // Quoted datum

type LBool = { tag: 'bool', value: boolean }
const lbool = (value: boolean): LBool => ({ tag: 'bool', value })

type LNum = { tag: 'num', value: number }
const lnum = (value: number): LNum => ({ tag: 'num', value })

type LChar = { 'tag' : 'char', 'value' : string }
const lchar = (value: string): LChar => ({ tag: 'char', value })

type LStr = { 'tag' : 'str', 'value' : string }
const lstr = (value: string): LStr => ({ tag: 'str', value })

export function litToValue (lit: Lit): Value {
  if (lit.tag === 'bool') {
    return lit.value
  } else if (lit.tag === 'num') {
    return lit.value
  } else if (lit.tag === 'char') {
    return vchar(lit.value)
  } else if (lit.tag === 'str') {
    return lit.value
  } else {
    // eslint-disable-next-line @typescript-eslint/restrict-template-expressions
    throw new ICE('litToValue', `unknown literal: ${lit}`)
  }
}

/** Expressions */
type Exp
  = EValue
  | EVar
  | ELit
  | ECall
  | ELam
  | EIf
  // | EAssign  // Assignment
  // | EMUse    // Macro use
  // | EMBlk    // Macro block
  // | EIncl    // Includer
  | ENil
  | EPair // N.B., an expression form to also serve as a value

  // Derived forms
  | ELet
  | ECond
  | EAnd
  | EOr
  | EBegin

  // Non-standard forms
  | EMatch // A pattern match expression

export type EValue = { tag: 'value', range: Range, value: Value, isValue: boolean, isList: boolean }
export const nlevalue = (value: Value): EValue => ({ tag: 'value', range: noRange(), value, isValue: true, isList: false })
export const unpackValue = (e: Exp): Value => (e as EValue).value
// N.B., this helper is useful for single stepping where we sometimes need to
// remove a layer of value wrapping.
export const unpackIfValue = (e: Exp): Value => e.tag === 'value' ? e.value : e

type EVar = { tag: 'var', range: Range, value: string, isValue: boolean, isList: boolean }
const evar = (range: Range, value: string): EVar => ({ tag: 'var', range, value, isValue: true, isList: false })
const nlevar = (value: string): EVar => evar(noRange(), value)

type ELit = { tag: 'lit', range: Range, value: Lit, isValue: boolean, isList: boolean }
const elit = (range: Range, value: Lit): ELit => ({ tag: 'lit', range, value, isValue: true, isList: false })

const ebool = (range: Range, value: boolean): ELit => elit(range, lbool(value))
const enumber = (range: Range, value: number): ELit => elit(range, lnum(value))
const echar = (range: Range, c: string): ELit => elit(range, lchar(c))
const estr = (range: Range, s: string): ELit => elit(range, lstr(s))

const nlebool = (value: boolean): ELit => elit(noRange(), lbool(value))
const nlenumber = (value: number): ELit => elit(noRange(), lnum(value))
const nlechar = (c: string): ELit => elit(noRange(), lchar(c))
const nlestr = (s: string): ELit => elit(noRange(), lstr(s))

type ECall = { tag: 'call', range: Range, head: Exp, args: Exp[], bracket: BracketKind, isValue: boolean, isList: boolean }
const ecall = (range: Range, head: Exp, args: Exp[], bracket: BracketKind = '('): ECall =>
  ({ tag: 'call', range, head, args, bracket, isValue: false, isList: false })
const nlecall = (head: Exp, args: Exp[]): ECall => ecall(noRange(), head, args)

type ELam = { tag: 'lam', range: Range, args: Name[], body: Exp, bracket: BracketKind, isValue: boolean, isList: boolean }
const elam = (range: Range, args: Name[], body: Exp, bracket: BracketKind = '('): ELam =>
  ({ tag: 'lam', range, args, body, bracket, isValue: true, isList: false })
const nlelam = (args: string[], body: Exp): ELam => elam(noRange(), args.map(nlname), body)

type EIf = { tag: 'if', range: Range, e1: Exp, e2: Exp, e3: Exp, bracket: BracketKind, isValue: boolean, isList: boolean }
const eif = (range: Range, e1: Exp, e2: Exp, e3: Exp, bracket: BracketKind = '('): EIf =>
  ({ tag: 'if', range, e1, e2, e3, bracket, isValue: false, isList: false })
const nleif = (e1: Exp, e2: Exp, e3: Exp): EIf => eif(noRange(), e1, e2, e3)

type ENil = { tag: 'nil', range: Range, isValue: boolean, isList: boolean }
const enil = (range: Range): ENil => ({ tag: 'nil', range, isValue: true, isList: true })
const nlenil = (): ENil => enil(noRange())

type EPair = { tag: 'pair', range: Range, e1: Exp, e2: Exp, bracket: BracketKind, isValue: boolean, isList: boolean }
const epair = (range: Range, e1: Exp, e2: Exp, bracket: BracketKind = '('): EPair =>
  ({ tag: 'pair', range, e1, e2, bracket, isValue: e1.isValue && e2.isValue, isList: e2.isList })
const nlepair = (e1: Exp, e2: Exp): EPair => epair(noRange(), e1, e2)

type LetKind = 'let' | 'let*' | 'letrec'

type ELet = { tag: 'let', range: Range, kind: LetKind, bindings: [Name, Exp][], body: Exp, bracket: BracketKind, isValue: boolean, isList: boolean }
// TODO: need to record individual BracketKinds for bindings
const elet = (range: Range, kind: LetKind, bindings: [Name, Exp][], body: Exp, bracket: BracketKind = '('): ELet =>
  ({ tag: 'let', kind, range, bindings, body, bracket, isValue: false, isList: false })
const nlelet = (kind: LetKind, bindings: [string, Exp][], body: Exp): ELet =>
  elet(noRange(), kind, bindings.map(b => [nlname(b[0]), b[1]]), body)

type ECond = { tag: 'cond', range: Range, branches: [Exp, Exp][], bracket: BracketKind, isValue: boolean, isList: boolean }
// TODO: need to record individual BracketKinds for branches
const econd = (range: Range, branches: [Exp, Exp][], bracket: BracketKind = '('): ECond =>
  ({ tag: 'cond', range, branches, bracket, isValue: false, isList: false })
const nlecond = (branches: [Exp, Exp][]): ECond => econd(noRange(), branches)

type EAnd = { tag: 'and', range: Range, args: Exp[], bracket: BracketKind, isValue: boolean, isList: boolean }
const eand = (range: Range, args: Exp[], bracket: BracketKind = '('): EAnd => ({ tag: 'and', range, args, bracket, isValue: false, isList: false })
const nleand = (args: Exp[]): EAnd => eand(noRange(), args)

type EOr = { tag: 'or', range: Range, args: Exp[], bracket: BracketKind, isValue: boolean, isList: boolean }
const eor = (range: Range, args: Exp[], bracket: BracketKind = '('): EOr => ({ tag: 'or', range, args, bracket, isValue: false, isList: false })
const nleor = (args: Exp[]): EOr => eor(noRange(), args)

type EBegin = { tag: 'begin', range: Range, exps: Exp[], bracket: BracketKind, isValue: boolean, isList: boolean }
export const ebegin = (range: Range, exps: Exp[], bracket: BracketKind = '('): EBegin =>
  ({ tag: 'begin', range, exps, bracket, isValue: false, isList: false })
export const nlebegin = (exps: Exp[]): EBegin => ebegin(noRange(), exps)

type EMatch = { tag: 'match', range: Range, scrutinee: Exp, branches: [Pat, Exp][], bracket: BracketKind, isValue: boolean, isList: boolean }
const ematch = (range: Range, scrutinee: Exp, branches: [Pat, Exp][], bracket: BracketKind): EMatch =>
  ({ tag: 'match', range, scrutinee, branches, bracket, isValue: false, isList: false })

// #endregion

// #region Expression pretty-printing

// function parens (ss: String[]) {
//   return `(${ss.join(' ')})`
// }

function litToString (l: Lit): string {
  switch (l.tag) {
    case 'bool': return l.value ? '#t' : '#f'
    case 'num': return l.value.toString()
    case 'char': return `#${l.value}`
    case 'str': return `"${l.value}"`
  }
}

function arrayToList (es: Exp[]): Exp {
  let ret: Exp = nlenil()
  for (let i = es.length - 1; i >= 0; i--) {
    ret = epair(es[i].range, es[i], ret)
  }
  return ret
}

function unsafeListToArray (e:Exp): Exp[] {
  const ret = []
  while (e.tag === 'pair') {
    ret.push(e.e1)
    e = e.e2
  }
  return ret
}

// function expToString (e:Exp): string {
//   switch (e.tag) {
//     case 'value': return valueToString(e.value)
//     case 'var': return e.value
//     case 'lit': return litToString(e.value)
//     case 'call': return parens([e.head].concat(e.args).map(expToString))
//     case 'lam': return parens(['lambda', parens(e.args.map(n => n.value)), expToString(e.body)])
//     case 'if': return parens(['if', expToString(e.e1), expToString(e.e2), expToString(e.e3)])
//     case 'nil': return 'null'
//     case 'pair':
//       return e.isList
//         ? parens(['list'].concat(unsafeListToArray(e).map(expToString)))
//         : parens(['cons', expToString(e.e1), expToString(e.e2)])
//     case 'let': return parens(['let', parens(e.bindings.map(([x, e]) => `(${x.value} ${expToString(e)})`)), expToString(e.body)])
//     case 'cond': return parens(['cond'].concat(e.branches.map(b => parens([expToString(b[0]), expToString(b[1])])).join(' ')))
//     case 'and': return parens(['and'].concat(parens(e.args.map(expToString))))
//     case 'or': return parens(['and'].concat(parens(e.args.map(expToString))))
//     case 'match':
//       return parens(['match', expToString(e.scrutinee)].concat(e.branches.map(b => parens([patToString(b[0]), expToString(b[1])]))))
//   }
// }

// function patToString (p: Pat): string {
//   switch (p.tag) {
//     case 'var': return p.id
//     case 'wild': return '_'
//     case 'null': return 'null'
//     case 'lit': return litToString(p.lit)
//     case 'ctor': return parens([p.head, ...p.args.map(patToString)])
//   }
// }

// #endregion

// #region Expression querying functions

function isValue (e:Exp): boolean {
  // N.B., vars are values because wrt single-step evaluation, they are environment
  // bound free-variables that stand in for values. We lazily substitute them in
  // runtime.js.
  return e.tag === 'value' || e.tag === 'var'
}

function nameEquals (n1: Name, n2: Name): boolean {
  return n1.value === n2.value
}

function litEquals (l1: Lit, l2: Lit): boolean {
  if (l1.tag === 'num' && l2.tag === 'num') {
    return l1.value === l2.value
  } else if (l1.tag === 'bool' && l2.tag === 'bool') {
    return l1.value === l2.value
  } else if (l1.tag === 'char' && l2.tag === 'char') {
    return l1.value === l2.value
  } else if (l1.tag === 'str' && l2.tag === 'str') {
    return l1.value === l2.value
  } else {
    return false
  }
}

function expEquals (e1: Exp, e2: Exp): boolean {
  if (e1.tag === 'var' && e2.tag === 'var') {
    return e1.value === e2.value
  } else if (e1.tag === 'lit' && e2.tag === 'lit') {
    return litEquals(e1.value, e2.value)
  } else if (e1.tag === 'call' && e2.tag === 'call') {
    return expEquals(e1.head, e2.head) &&
      e1.args.length === e2.args.length &&
      e1.args.every((e, i) => expEquals(e, e2.args[i]))
  } else if (e1.tag === 'lam' && e2.tag === 'lam') {
    return e1.args.length === e2.args.length &&
      e1.args.every((x, i) => nameEquals(x, e2.args[i])) &&
      expEquals(e1.body, e2.body)
  } else if (e1.tag === 'if' && e2.tag === 'if') {
    return expEquals(e1.e1, e2.e1) &&
      expEquals(e1.e2, e2.e2) &&
      expEquals(e1.e3, e2.e3)
  } else if (e1.tag === 'nil' && e2.tag === 'nil') {
    return true
  } else if (e1.tag === 'pair' && e2.tag === 'pair') {
    return expEquals(e1.e1, e2.e1) && expEquals(e1.e2, e2.e2)
  } else if (e1.tag === 'let' && e2.tag === 'let') {
    return e1.kind === e2.kind && e1.bindings.length === e2.bindings.length &&
      e1.bindings.every(([x, e], i) => nameEquals(x, e2.bindings[i][0]) && expEquals(e, e2.bindings[i][1])) &&
      expEquals(e1.body, e2.body)
    // TODO: need cases for all the other value-style expression forms!
    // TODO: also need cases for match!
  } else {
    return false
  }
}

// #endregion

// #region Patterns

type Pat = PVar | PWild | PNull | PLit | PCtor

type PVar = { tag: 'var', id: string, range: Range }
const pvar = (range: Range, id: string): PVar => ({ tag: 'var', id, range })

type PWild = { tag: 'wild', range: Range }
const pwild = (range: Range): PWild => ({ tag: 'wild', range })

type PNull = { tag: 'null', range: Range }
const pnull = (range: Range): PNull => ({ tag: 'null', range })

type PLit = { tag: 'lit', lit: Lit, range: Range }
const plit = (range: Range, lit: Lit): PLit => ({ tag: 'lit', lit, range })

type PCtor = { tag: 'ctor', head: string, args: Pat[], range: Range }
const pctor = (range: Range, head: string, args: Pat[]): PCtor => ({ tag: 'ctor', head, args, range })

function fvarsOfPat (p: Pat): string[] {
  switch (p.tag) {
    case 'var':
      return [p.id]
    case 'wild':
      return []
    case 'null':
      return []
    case 'lit':
      return []
    case 'ctor':
      return p.args.flatMap(fvarsOfPat)
  }
}

// #endregion

// #region Statement and program forms

export type SEffect = SImported | SError | SBinding | STestResult | SValue

type SImported = { tag: 'imported', range: Range, source: string }
const simported = (source: string, range: Range = noRange()): SImported => ({ tag: 'imported', range, source })

type SError = { tag: 'error', range: Range, errors: ErrorDetails[] }
const serror = (errors: ErrorDetails[], range: Range = noRange()): SEffect => ({ tag: 'error', range, errors })

type SBinding = { tag: 'binding', range: Range, name: string }
const sbinding = (name: string, range: Range = noRange()): SBinding => ({ tag: 'binding', range, name })

type STestResult = {
  tag: 'testresult',
  range: Range,
  desc: string,
  passed: boolean,
  reason?: string,
  expected?: Exp,
  actual?: Exp
}
const stestresult = (desc: string, passed: boolean, reason?: string, expected?: Exp, actual?: Exp, range: Range = noRange()): STestResult =>
  ({ tag: 'testresult', range, desc, passed, reason, expected, actual })

type SValue = { tag: 'value', range: Range, output?: string }
const svalue = (output?: string, range: Range = noRange()): SValue => ({ tag: 'value', range, output })

type Stmt = SImport | SDefine | SExp | SStruct | STestCase | SEffect

type SImport = { tag: 'import', range: Range, source: string }
const simport = (source: string, range: Range): SImport => ({ tag: 'import', range, source })

type SDefine = { tag: 'define', range: Range, name: Name, value: Exp }
const sdefine = (name: Name, value: Exp, range: Range = noRange()): SDefine => ({ tag: 'define', range, name, value })

type SStruct = { tag: 'struct', range: Range, id: Name, fields: Name[] }
const sstruct = (id: Name, fields: Name[], range: Range = noRange()): SStruct => ({ tag: 'struct', range, id, fields })

type STestCase = { tag: 'testcase', range: Range, desc: Exp, comp: Exp, expected: Exp, actual: Exp }
const stestcase = (desc: Exp, comp: Exp, expected: Exp, actual: Exp, range: Range = noRange()): STestCase =>
  ({ tag: 'testcase', range, desc, comp, expected, actual })

type SExp = { tag: 'exp', range: Range, value: Exp }
const sexp = (value: Exp, range: Range = noRange()): SExp => ({ tag: 'exp', range, value })

type Program = Stmt[]

// #endregion

// #region Statement and program pretty-printing

// function stmtToString (stmt: Stmt, outputBindings: boolean = false): string {
//   switch (stmt.tag) {
//     case 'define': return `(define ${stmt.name.value} ${expToString(stmt.value)})`
//     case 'exp': return expToString(stmt.value)
//     case 'struct': return `(struct ${stmt.id.value} (${stmt.fields.map(f => f.value).join(' ')}))`
//     case 'testcase': return `(test-case ${expToString(stmt.desc)} ${expToString(stmt.comp)} ${expToString(stmt.expected)} ${expToString(stmt.actual)})`
//     case 'import': return `(import ${stmt.source})`
//     case 'error': return stmt.errors.map(err => `[[error: ${err.message}]]`).join('\n')
//     case 'binding': return outputBindings ? `[[${stmt.name} bound]]` : ''
//     case 'testresult': {
//       if (stmt.passed) {
//         return `[[ Test "${stmt.desc}": passed! ]]`
//       } else {
//         const msg: string = stmt.reason
//           ? stmt.reason
//           : `Expected: ${expToString(stmt.expected!)}\nActual: ${expToString(stmt.actual!)}`
//         return `[[ Test "${stmt.desc}": failed!\n${msg}]]`
//       }
//     }
//     case 'value': return valueToString(stmt.value)
//     case 'imported': return outputBindings ? `[[${stmt.source} imported]]` : ''
//   }
// }

// function progToString (prog: Program, outputBindings: boolean = false): string {
//   return `${prog.map(s => stmtToString(s, outputBindings)).filter(s => s.length > 0).join('\n\n')}`
// }

// #endregion

// #region Statement and program querying functions

function isOutputEffect (stmt: Stmt): boolean {
  return stmt.tag === 'error' || stmt.tag === 'value'
}

function isStmtDone (stmt: Stmt): boolean {
  return stmt.tag === 'error' || stmt.tag === 'binding' || stmt.tag === 'value' || stmt.tag === 'imported' || stmt.tag === 'testresult'
}

function indexOfCurrentStmt (prog: Program): number {
  for (let i = 0; i < prog.length; i++) {
    if (!isStmtDone(prog[i])) {
      return i
    }
  }
  return -1
}

// #endregion

export {
  Prim, Name, name, nlname,
  Lit, LBool, LNum, LChar, LStr,
  Exp, EVar, ELit, ECall, ELam, EIf, ENil, EPair, LetKind, ELet, ECond, EAnd, EOr,
  lbool, lnum, lchar, lstr, ebool, enumber, echar, estr,
  evar, elit, ecall, elam, eif, elet, enil, epair, econd, eand, eor, ematch,
  nlebool, nlenumber, nlechar, nlestr,
  nlevar, nlecall, nlelam, nleif, nlelet, nlenil, nlepair, nlecond, nleand, nleor,
  litToString, arrayToList, unsafeListToArray, litEquals, expEquals,
  isValue, Pat, PVar, PWild, PNull, PLit, PCtor, pvar, pwild, pnull, plit, pctor, fvarsOfPat,
  Stmt, serror, sbinding, svalue, simported, sdefine, stestresult, sstruct, stestcase, sexp, isOutputEffect, isStmtDone, simport,
  Program, indexOfCurrentStmt
}
