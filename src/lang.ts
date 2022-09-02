import { Range, noRange } from './loc.js'
import { ErrorDetails, Result } from './result.js'

// #region Names

export type BracketKind = '(' | '[' | '{'

type Name = { value: string, range: Range }
const name = (value: string, range: Range): Name => ({ value, range })
const nlname = (value: string): Name => name(value, noRange())

// #endregion

// #region Expression forms

/* eslint-disable no-use-before-define */

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
  constructor (public sig: string, public args: string[], public desc: string) { }

  /**
   * @returns A string containing the docstring formatted in Markdown.
   */
  public docToMarkdown(): string {
  return `
~~~
${this.sig.trim()}

${this.args.map(arg => '  ' + arg.trim()).join('\n')}
~~~

${this.desc.trim()}
  `.trim()
  }
}

export type EnvEntry = { value: Exp, source: string, range?: Range, doc?: Doc }
export const entry = (value: Exp, source: string, range?: Range, doc?: Doc): EnvEntry =>
  ({ value, source, range, doc})

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
 * @param args - the arguments passed to the primitive, assumed to be values.
 * @param app - the full application expression, for error-reporting purposes.
 */
type Prim = (env:Env, args: Exp[], app: Exp) => Result<Exp>

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

/** Expressions */
type Exp
  = EVar
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
  | EStruct // An object created from a struct constructor
  | EObj // A wrapped, tagged Javascript object
  | EPrim // A primitive function value

/* eslint-enable */

type EVar = { tag: 'var', range: Range, value: string }
const evar = (range: Range, value: string): EVar => ({ tag: 'var', range, value })
const nlevar = (value: string): EVar => evar(noRange(), value)

type ELit = { tag: 'lit', range: Range, value: Lit }
const elit = (range: Range, value: Lit): ELit => ({ tag: 'lit', range, value })

const ebool = (range: Range, value: boolean): ELit => elit(range, lbool(value))
const enumber = (range: Range, value: number): ELit => elit(range, lnum(value))
const echar = (range: Range, c: string): ELit => elit(range, lchar(c))
const estr = (range: Range, s: string): ELit => elit(range, lstr(s))

const nlebool = (value: boolean): ELit => elit(noRange(), lbool(value))
const nlenumber = (value: number): ELit => elit(noRange(), lnum(value))
const nlechar = (c: string): ELit => elit(noRange(), lchar(c))
const nlestr = (s: string): ELit => elit(noRange(), lstr(s))

type ECall = { tag: 'call', range: Range, head: Exp, args: Exp[], bracket: BracketKind }
const ecall = (range: Range, head: Exp, args: Exp[], bracket: BracketKind = '('): ECall =>
  ({ tag: 'call', range, head, args, bracket })
const nlecall = (head: Exp, args: Exp[]): ECall => ecall(noRange(), head, args)

type ELam = { tag: 'lam', range: Range, args: Name[], body: Exp, bracket: BracketKind }
const elam = (range: Range, args: Name[], body: Exp, bracket: BracketKind = '('): ELam =>
  ({ tag: 'lam', range, args, body, bracket })
const nlelam = (args: string[], body: Exp): ELam => elam(noRange(), args.map(nlname), body)

type EIf = { tag: 'if', range: Range, e1: Exp, e2: Exp, e3: Exp, bracket: BracketKind }
const eif = (range: Range, e1: Exp, e2: Exp, e3: Exp, bracket: BracketKind = '('): EIf =>
  ({ tag: 'if', range, e1, e2, e3, bracket })
const nleif = (e1: Exp, e2: Exp, e3: Exp): EIf => eif(noRange(), e1, e2, e3)

type ENil = { tag: 'nil', range: Range }
const enil = (range: Range): ENil => ({ tag: 'nil', range })
const nlenil = (): ENil => enil(noRange())

type EPair = { tag: 'pair', range: Range, e1: Exp, e2: Exp, bracket: BracketKind }
const epair = (range: Range, e1: Exp, e2: Exp, bracket: BracketKind = '('): EPair =>
  ({ tag: 'pair', range, e1, e2, bracket })
const nlepair = (e1: Exp, e2: Exp): EPair => epair(noRange(), e1, e2)

type ELet = { tag: 'let', range: Range, bindings: [Name, Exp][], body: Exp, bracket: BracketKind }
// TODO: need to record individual BracketKinds for bindings
const elet = (range: Range, bindings: [Name, Exp][], body: Exp, bracket: BracketKind = '('): ELet =>
  ({ tag: 'let', range, bindings, body, bracket })
const nlelet = (bindings: [string, Exp][], body: Exp): ELet => elet(noRange(), bindings.map(b => [nlname(b[0]), b[1]]), body)

type ECond = { tag: 'cond', range: Range, branches: [Exp, Exp][], bracket: BracketKind }
// TODO: need to record individual BracketKinds for branches
const econd = (range: Range, branches: [Exp, Exp][], bracket: BracketKind = '('): ECond =>
  ({ tag: 'cond', range, branches, bracket })
const nlecond = (branches: [Exp, Exp][]): ECond => econd(noRange(), branches)

type EAnd = { tag: 'and', range: Range, args: Exp[], bracket: BracketKind }
const eand = (range: Range, args: Exp[], bracket: BracketKind = '('): EAnd => ({ tag: 'and', range, args, bracket })
const nleand = (args: Exp[]): EAnd => eand(noRange(), args)

type EOr = { tag: 'or', range: Range, args: Exp[], bracket: BracketKind }
const eor = (range: Range, args: Exp[], bracket: BracketKind = '('): EOr => ({ tag: 'or', range, args, bracket })
const nleor = (args: Exp[]): EOr => eor(noRange(), args)

type EStruct = { tag: 'struct', range: Range, kind: string, obj: object }
const nlestruct = (kind: string, obj: object): EStruct => ({ tag: 'struct', range: noRange(), kind, obj })

type EObj = { tag: 'obj', range: Range, kind: string, obj: object }
const nleobj = (kind: string, obj: object): EObj => ({ tag: 'obj', range: noRange(), kind, obj })

type EPrim = { tag: 'prim', range: Range, prim: Prim }
const nleprim = (prim: Prim): EPrim => ({ tag: 'prim', range: noRange(), prim })

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
    case 'let': return parens(['let', parens(e.bindings.map(([x, e]) => `(${x} ${expToString(e)})`)), expToString(e.body)])
    case 'cond': return parens(['cond'].concat(e.branches.map(b => parens([expToString(b[0]), expToString(b[1])])).join(' ')))
    case 'and': return parens(['and'].concat(parens(e.args.map(expToString))))
    case 'or': return parens(['and'].concat(parens(e.args.map(expToString))))
    case 'struct': return `[struct ${(e.obj as any).kind}]`
    case 'obj': return `[object ${e.kind}]`
    case 'prim': return `[prim ${e.prim.name}]`
  }
}

// #endregion

// #region Expression querying functions

function isValue (e:Exp): boolean {
  switch (e.tag) {
    case 'var': return true
    case 'lit': return true
    case 'call': return false
    case 'lam': return true
    case 'if': return false
    case 'nil': return true
    case 'pair': return isValue(e.e1) && isValue(e.e2)
    case 'let': return false
    case 'cond': return false
    case 'and': return false
    case 'or': return false
    case 'struct': return true
    case 'obj': return true
    case 'prim': return true
  }
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
  while (e.tag === 'pair') {
    e = e.e2
  }
  return e.tag === 'nil'
}

function isPrim (e:Exp): boolean {
  return e.tag === 'prim'
}

function isStruct (e: Exp): boolean {
  return e.tag === 'struct'
}

function isStructKind (e: Exp, kind: string): boolean {
  return e.tag === 'struct' && e.kind === `${kind}`
}

function isObj (e: Exp): boolean {
  return e.tag === 'obj'
}

function isObjKind (e: Exp, kind: string): boolean {
  return e.tag === 'obj' && e.kind === kind
}

function isProcedure (e: Exp): boolean {
  return isLambda(e) || isPrim(e)
}

function asNum_ (e:Exp): number {
  return ((e as ELit).value as LNum).value
}

function asBool_ (e: Exp): boolean {
  return ((e as ELit).value as LBool).value
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

function asObj_ (e: Exp): object {
  return (e as EObj).obj
}

function fromObj_ <T> (e: Exp): T {
  return ((e as EObj).obj as unknown) as T
}

function asStruct_ (e: Exp): object {
  return (e as EStruct).obj
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
    return e1.bindings.length === e2.bindings.length &&
      e1.bindings.every(([x, e], i) => nameEquals(x, e2.bindings[i][0]) && expEquals(e, e2.bindings[i][1])) &&
      expEquals(e1.body, e2.body)
    // TODO: need cases for all the other value-style expression forms!
  } else {
    return false
  }
}

// #endregion

// #region Statement and program forms

/* eslint-disable no-use-before-define */
type SEffect = SImported | SError | SBinding | SValue
/* eslint-enable */

type SImported = { tag: 'imported', source: string }
const simported = (source: string): SImported => ({ tag: 'imported', source })

type SError = { tag: 'error', errors: ErrorDetails[] }
const serror = (errors: ErrorDetails[]): SEffect => ({ tag: 'error', errors })

type SBinding = { tag: 'binding', name: string }
const sbinding = (name: string): SBinding => ({ tag: 'binding', name })

type SValue = { tag: 'value', value: Exp }
const svalue = (value: Exp): SValue => ({ tag: 'value', value })

/* eslint-disable no-use-before-define */
type Stmt = SImport | SDefine | SExp | SStruct | SEffect
/* eslint-enable */

type SImport = { tag: 'import', range: Range, source: string }
const simport = (range: Range, source: string): SImport => ({ tag: 'import', range, source })

type SDefine = { tag: 'define', name: Name, value: Exp }
const sdefine = (name: Name, value: Exp): SDefine => ({ tag: 'define', name, value })

type SStruct = { tag: 'struct', id: Name, fields: Name[] }
const sstruct = (id: Name, fields: Name[]): SStruct => ({ tag: 'struct', id, fields })

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
    case 'import': return `(import ${stmt.source})`
    case 'error': return stmt.errors.map(err => `[[error: ${err.message}]]`).join('\n')
    case 'binding': return outputBindings ? `[[${stmt.name} bound]]` : ''
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
  return stmt.tag === 'error' || stmt.tag === 'binding' || stmt.tag === 'value' || stmt.tag === 'imported'
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
  Exp, EVar, ELit, ECall, ELam, EIf, ENil, EPair, ELet, ECond, EAnd, EOr, EStruct, EObj,
  lbool, lnum, lchar, lstr, ebool, enumber, echar, estr,
  evar, elit, ecall, elam, eif, elet, enil, epair, econd, eand, eor,
  nlebool, nlenumber, nlechar, nlestr,
  nlevar, nlecall, nlelam, nleif, nlelet, nlenil, nlepair, nlecond, nleand, nleor, nlestruct, nleobj, nleprim,
  litToString, arrayToList, unsafeListToArray, expToString, expEquals,
  isValue, isNumber, isInteger, isReal, isBoolean, isString, isChar, isLambda, isPair, isList, isPrim, isObj, isStruct, isStructKind, isObjKind, isProcedure,
  asNum_, asBool_, asString_, asList_, asPair_, asStruct_, asObj_, fromObj_,
  Stmt, serror, sbinding, svalue, simported, sdefine, sstruct, sexp, isOutputEffect, isStmtDone, stmtToString, simport,
  Program, indexOfCurrentStmt, progToString
}
