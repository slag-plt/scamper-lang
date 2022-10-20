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
export type LambdaType = { tag: 'lambda', args: Name[], body: Exp }
export type PrimType = { tag: 'prim', fn: Prim }
export type FunctionType = LambdaType | PrimType
export type CharType = { tag: 'char', value: string }
export type PairType = { tag: 'pair', fst: Value, snd: Value }
export type StructType = { tag: 'struct', kind: String, fields: Map<string, Value> }

/** In Scamper, a Value is, directly, a Javascript value. */
export type Value = boolean | number | string | null | object | Value[] | TaggedObject

/*
 * Scamper-Javascript value conversion:
 *
 * (boolean? e) <=> typeof e === 'boolean'
 * (number? e) <=> typeof e === 'number'
 * (string? e) <==> typeof e === 'string'
 * (null? e) <==> e === null
 * (char? e) <==> typeof e === 'object': { tag: 'char', value: string }  <-- need to deprecate this!
 * (function? e) <==> typeof e === 'object': { tag: 'lambda', args: Name[], body: Exp } or { tag: 'prim', fn: Prim }
 * (pair? e) <==> typeof e === 'object': { tag: 'pair', fst: Value, snd: Value }
 * (struct? e) <==> typeof e === 'object': { tag: 'struct', 'kind': string, fields: Map<string, Value> }
 * (object? e) <==> typeof e === 'object': { ... }
 * (vector? e) <==> Array.isArray(e)
 *
 * TODO: do we need to "uniquify" the fields so that, e.g., an object can't
 * mock-up a struct?
 */

export const vchar = (value: string): Value => ({ tag: 'char', value })
export const vlambda = (args: Name[], body: Exp): Value => ({ tag: 'lambda', args, body })
export const vprim = (fn: Prim): Value => ({ tag: 'prim', fn })
export const vpair = (fst: Value, snd: Value): Value => ({ tag: 'pair', fst, snd })
export const vstruct = (kind: string, fields: Map<string, Value>): Value => ({ tag: 'struct', kind, fields })

export const isTaggedObject = (v: Value): boolean =>
  typeof v === 'object' && Object.hasOwn(v as object, 'tag')
export const valueIsBoolean = (v: Value): boolean => typeof v === 'boolean'
export const valueIsNumber = (v: Value): boolean => typeof v === 'number'
export const valueIsString = (v: Value): boolean => typeof v === 'string'
export const valueIsNull = (v: Value): boolean => v === null
export const valueIsChar = (v: Value): boolean => isTaggedObject(v) && (v as TaggedObject).tag === 'char'
export const valueIsLambda = (v: Value): boolean => isTaggedObject(v) && (v as TaggedObject).tag === 'lambda'
export const valueIsPrim = (v: Value): boolean => isTaggedObject(v) && (v as TaggedObject).tag === 'prim'
export const valueIsFunction = (v: Value): boolean => valueIsLambda(v) || valueIsPrim(v)
export const valueIsPair = (v: Value): boolean => isTaggedObject(v) && (v as TaggedObject).tag === 'pair'
export const valueIsStruct = (v: Value): boolean => isTaggedObject(v) && (v as TaggedObject).tag === 'struct'
export const valueIsStructKind = (v: Value, kind: string): boolean =>
  valueIsStruct(v) && (v as StructType).kind === kind
export const valueIsObject = (v: Value): boolean => typeof v === 'object'
export const valueIsVector = (v: Value): boolean => Array.isArray(v)

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
      throw new ICE('valueListToArray', `valueListToArray: not a list: ${v.toString()}`)
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
      : `(pair ${valueToString((v as PairType).fst)} ${valueToString((v as PairType).snd)})`
  } else if (valueIsStruct(v)) {
    return `(struct ${(v as StructType).kind.toString()} ${[...(v as StructType).fields.values()].map(v => valueToString(v)).join(' ')})`
  } else if (Array.isArray(v)) {
    throw new ICE('valueToString', 'vector not yet implemented')
  } else if (typeof v === 'object') {
    return '[object Object]'
  }
  // eslint-disable-next-line @typescript-eslint/restrict-template-expressions
  throw new ICE('valueToString', `unknown value encountered: ${v}`)
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
  entries: Map<string, EnvEntry>

  constructor (entries?: Iterable<[string, EnvEntry]>) {
    if (entries) {
      this.entries = new Map(entries)
    } else {
      this.entries = new Map()
    }
  }

  public has (key: string): boolean {
    return this.entries.has(key)
  }

  public get (key: string): EnvEntry | undefined {
    return this.entries.get(key)
  }

  public items (): Iterable<[string, EnvEntry]> {
    return this.entries.entries()
  }

  public names (): Iterable<string> {
    return this.entries.keys()
  }

  public append (key: string, value: EnvEntry): Env {
    return new Env([...this.entries, [key, value]])
  }

  public concat (other: Env): Env {
    return new Env([...this.entries, ...other.entries])
  }

  public without (keys: string[]): Env {
    const ret = new Env(this.items())
    keys.forEach(k => ret.entries.delete(k))
    return ret
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
  // | EBegin   // TODO: implement me

  // Non-standard forms
  | EMatch // A pattern match expression

type EValue = { tag: 'value', range: Range, value: Value, isValue: boolean, isList: boolean }
export const nlevalue = (value: Value): EValue => ({ tag: 'value', range: noRange(), value, isValue: true, isList: false })

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

type EMatch = { tag: 'match', range: Range, scrutinee: Exp, branches: [Pat, Exp][], bracket: BracketKind, isValue: boolean, isList: boolean }
const ematch = (range: Range, scrutinee: Exp, branches: [Pat, Exp][], bracket: BracketKind): EMatch =>
  ({ tag: 'match', range, scrutinee, branches, bracket, isValue: false, isList: false })

// #endregion

// #region Expression pretty-printing

function parens (ss: String[]) {
  return `(${ss.join(' ')})`
}

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

function expToString (e:Exp): string {
  switch (e.tag) {
    case 'value': return valueToString(e.value)
    case 'var': return e.value
    case 'lit': return litToString(e.value)
    case 'call': return parens([e.head].concat(e.args).map(expToString))
    case 'lam': return parens(['lambda', parens(e.args.map(n => n.value)), expToString(e.body)])
    case 'if': return parens(['if', expToString(e.e1), expToString(e.e2), expToString(e.e3)])
    case 'nil': return 'null'
    case 'pair':
      return isList(e)
        ? parens(['list'].concat(unsafeListToArray(e).map(expToString)))
        : parens(['cons', expToString(e.e1), expToString(e.e2)])
    case 'let': return parens(['let', parens(e.bindings.map(([x, e]) => `(${x.value} ${expToString(e)})`)), expToString(e.body)])
    case 'cond': return parens(['cond'].concat(e.branches.map(b => parens([expToString(b[0]), expToString(b[1])])).join(' ')))
    case 'and': return parens(['and'].concat(parens(e.args.map(expToString))))
    case 'or': return parens(['and'].concat(parens(e.args.map(expToString))))
    case 'match':
      return parens(['match', expToString(e.scrutinee)].concat(e.branches.map(b => parens([patToString(b[0]), expToString(b[1])]))))
  }
}

function patToString (p: Pat): string {
  switch (p.tag) {
    case 'var': return p.id
    case 'wild': return '_'
    case 'null': return 'null'
    case 'lit': return litToString(p.lit)
    case 'ctor': return parens([p.head, ...p.args.map(patToString)])
  }
}

// #endregion

// #region Expression querying functions

function isValue (e:Exp): boolean {
  return e.tag === 'value'
}

function isNumber (e:Exp): boolean {
  return e.tag === 'lit' && e.value.tag === 'num'
}

function isInteger (e:Exp): boolean {
  return e.tag === 'lit' && e.value.tag === 'num' && Number.isInteger(e.value.value)
}

function isReal (e:Exp): boolean {
  return e.tag === 'lit' && e.value.tag === 'num' && !Number.isInteger(e.value.value)
}

function isBoolean (e:Exp): boolean {
  return e.tag === 'lit' && e.value.tag === 'bool'
}

function isString (e:Exp): boolean {
  return e.tag === 'lit' && e.value.tag === 'str'
}

function isChar (e:Exp): boolean {
  return e.tag === 'lit' && e.value.tag === 'char'
}

function isLambda (e:Exp): boolean {
  return e.tag === 'lam'
}

function isPair (e:Exp): boolean {
  return e.tag === 'pair'
}

function isList (e:Exp): boolean {
  return e.isList
}

function asNum_ (e:Exp): number {
  return ((e as ELit).value as LNum).value
}

function asBool_ (e: Exp): boolean {
  return ((e as ELit).value as LBool).value
}

function asChar_ (e: Exp): string {
  return ((e as ELit).value as LChar).value
}

function asString_ (e: Exp): string {
  return ((e as ELit).value as LStr).value
}

function asList_ (e: Exp): Exp[] {
  return unsafeListToArray(e)
}

function asPair_ (e: Exp): [Exp, Exp] {
  return [(e as EPair).e1, (e as EPair).e2]
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

type SEffect = SImported | SError | SBinding | STestResult | SValue

type SImported = { tag: 'imported', source: string }
const simported = (source: string): SImported => ({ tag: 'imported', source })

type SError = { tag: 'error', errors: ErrorDetails[] }
const serror = (errors: ErrorDetails[]): SEffect => ({ tag: 'error', errors })

type SBinding = { tag: 'binding', name: string }
const sbinding = (name: string): SBinding => ({ tag: 'binding', name })

type STestResult = {
  tag: 'testresult',
  desc: string,
  passed: boolean,
  reason?: string,
  expected?: Exp,
  actual?: Exp
}
const stestresult = (desc: string, passed: boolean, reason?: string, expected?: Exp, actual?: Exp): STestResult =>
  ({ tag: 'testresult', desc, passed, reason, expected, actual })

type SValue = { tag: 'value', value: Exp }
const svalue = (value: Exp): SValue => ({ tag: 'value', value })

type Stmt = SImport | SDefine | SExp | SStruct | STestCase | SEffect

type SImport = { tag: 'import', range: Range, source: string }
const simport = (range: Range, source: string): SImport => ({ tag: 'import', range, source })

type SDefine = { tag: 'define', name: Name, value: Exp }
const sdefine = (name: Name, value: Exp): SDefine => ({ tag: 'define', name, value })

type SStruct = { tag: 'struct', id: Name, fields: Name[] }
const sstruct = (id: Name, fields: Name[]): SStruct => ({ tag: 'struct', id, fields })

type STestCase = { tag: 'testcase', desc: Exp, comp: Exp, expected: Exp, actual: Exp }
const stestcase = (desc: Exp, comp: Exp, expected: Exp, actual: Exp): STestCase => ({ tag: 'testcase', desc, comp, expected, actual })

type SExp = { tag: 'exp', value: Exp }
const sexp = (value: Exp): SExp => ({ tag: 'exp', value })

type Program = { statements: Stmt[] }

// #endregion

// #region Statement and program pretty-printing

function stmtToString (stmt: Stmt, outputBindings: boolean = false): string {
  switch (stmt.tag) {
    case 'define': return `(define ${stmt.name.value} ${expToString(stmt.value)})`
    case 'exp': return expToString(stmt.value)
    case 'struct': return `(struct ${stmt.id.value} (${stmt.fields.map(f => f.value).join(' ')}))`
    case 'testcase': return `(test-case ${expToString(stmt.desc)} ${expToString(stmt.comp)} ${expToString(stmt.expected)} ${expToString(stmt.actual)})`
    case 'import': return `(import ${stmt.source})`
    case 'error': return stmt.errors.map(err => `[[error: ${err.message}]]`).join('\n')
    case 'binding': return outputBindings ? `[[${stmt.name} bound]]` : ''
    case 'testresult': {
      if (stmt.passed) {
        return `[[ Test "${stmt.desc}": passed! ]]`
      } else {
        const msg: string = stmt.reason
          ? stmt.reason
          : `Expected: ${expToString(stmt.expected!)}\nActual: ${expToString(stmt.actual!)}`
        return `[[ Test "${stmt.desc}": failed!\n${msg}]]`
      }
    }
    case 'value': return expToString(stmt.value)
    case 'imported': return outputBindings ? `[[${stmt.source} imported]]` : ''
  }
}

function progToString (prog: Program, outputBindings: boolean = false): string {
  return `${prog.statements.map(s => stmtToString(s, outputBindings)).filter(s => s.length > 0).join('\n\n')}`
}

// #endregion

// #region Statement and program querying functions

function isOutputEffect (stmt: Stmt): boolean {
  return stmt.tag === 'error' || stmt.tag === 'value'
}

function isStmtDone (stmt: Stmt): boolean {
  return stmt.tag === 'error' || stmt.tag === 'binding' || stmt.tag === 'value' || stmt.tag === 'imported' || stmt.tag === 'testresult'
}

function indexOfCurrentStmt (prog: Program): number {
  for (let i = 0; i < prog.statements.length; i++) {
    if (!isStmtDone(prog.statements[i])) {
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
  litToString, arrayToList, unsafeListToArray, expToString, litEquals, expEquals,
  isValue, isNumber, isInteger, isReal, isBoolean, isString, isChar, isLambda, isPair, isList,
  asNum_, asBool_, asChar_, asString_, asList_, asPair_,
  Pat, PVar, PWild, PNull, PLit, PCtor, pvar, pwild, pnull, plit, pctor, fvarsOfPat,
  Stmt, serror, sbinding, svalue, simported, sdefine, stestresult, sstruct, stestcase, sexp, isOutputEffect, isStmtDone, stmtToString, simport,
  Program, indexOfCurrentStmt, progToString
}
