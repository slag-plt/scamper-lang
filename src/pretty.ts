import * as L from './lang.js'
import { detailsToCompleteString, ICE } from './result.js'
import * as P from './parser.js'
import * as Runtime from './runtime.js'

// N.B., Because we output to HTML, we must take care to escape HTML entities.
// But we can't escape all the entities---we want to leave the angle brackets
// that should ACTUALLY be interpreted as html tags! In the future, we should
// rely on hacky two-phase output to render html, but for now, we have to
// do a "deep" sanitization of entities, escaping in all cases except when we
// emit actual html tags.
const sanitize = (s: string, htmlOutput: boolean): string =>
  htmlOutput ? s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;') : s

const namedCharTable = new Map(Array.from(P.namedCharValues.entries()).map(([name, value]) => [value, name]))

function charToName (s: string): string {
  return namedCharTable.get(s) ?? s
}

function litToString (l: L.Lit, htmlOutput: boolean): string {
  switch (l.tag) {
    case 'bool': return l.value ? '#t' : '#f'
    case 'num': return l.value.toString()
    case 'char': return `#\\${sanitize(charToName(l.value), htmlOutput)}`
    case 'str': return `"${sanitize(l.value, htmlOutput)}"`
  }
}

function isSimpleExp (e: L.Exp): boolean {
  switch (e.tag) {
    case 'value': return true
    case 'var': return true
    case 'lit': return true
    case 'call': return e.args.every(isSimpleExp)
    case 'lam': return false
    case 'if': return false
    case 'nil': return true
    case 'pair': return true
    case 'let': return false
    case 'cond': return false
    case 'and': return true
    case 'or': return true
    case 'match': return false
    case 'begin': return false
  }
}

function nestingDepth (e: L.Exp): number {
  switch (e.tag) {
    case 'value': return 0
    case 'var': return 0
    case 'lit': return 0
    case 'call': return 1 + Math.max(...e.args.map(nestingDepth))
    case 'lam': return 1 + nestingDepth(e.body)
    case 'if': return 1 + Math.max(nestingDepth(e.e1), nestingDepth(e.e2), nestingDepth(e.e3))
    case 'nil': return 0
    case 'pair': return 1 + Math.max(nestingDepth(e.e1), nestingDepth(e.e2))
    case 'let': return 0 // TODO
    case 'cond': return 0 // TODO
    case 'and': return 0 // TODO
    case 'or': return 0 // TODO
    case 'match': return 0 // TODO
    case 'begin': return 0 // TODO
  }
}

function parens (bracketKind: L.BracketKind, ss: string[], sep: string = ' '): string {
  switch (bracketKind) {
    case '(':
      return `(${ss.join(sep)})`
    case '{':
      return `{${ss.join(sep)}}`
    case '[':
      return `[${ss.join(sep)}]`
  }
}

function indent (col: number, s: string): string {
  return `${' '.repeat(col)}${s}`
}

function patToString (p: L.Pat, htmlOutput: boolean): string {
  switch (p.tag) {
    case 'var': return sanitize(p.id, htmlOutput)
    case 'wild': return '_'
    case 'null': return 'null'
    case 'lit': return litToString(p.lit, htmlOutput)
    case 'ctor': return parens('(', [p.head, ...p.args.map(p => patToString(p, htmlOutput))])
  }
}

export function valueToString (col: number, v: L.Value, htmlOutput: boolean = false): string {
  if (typeof v === 'boolean') {
    return v ? '#t' : '#f'
  } else if (typeof v === 'number') {
    return v.toString()
  } else if (typeof v === 'string') {
    return `"${sanitize(v, htmlOutput)}"`
  } else if (L.valueIsChar(v)) {
    const ch = (v as L.CharType).value
    let printed = ch
    switch (ch) {
      // TODO: probably add in special cases for... all the other cases!
      case ' ': printed = 'space'; break
      default: break
    }
    return `#\\${sanitize(printed, htmlOutput)}`
  } else if (L.valueIsLambda(v) || L.valueIsPrim(v)) {
    return '[object Function]'
  } else if (L.valueIsPair(v)) {
    return L.valueIsList(v)
      ? `(list ${L.valueListToArray_(v).map(v => valueToString(col, v, htmlOutput)).join(' ')})`
      : `(cons ${valueToString(col, (v as L.PairType).fst, htmlOutput)} ${valueToString(col, (v as L.PairType).snd, htmlOutput)})`
  } else if (L.valueIsStruct(v)) {
    const s = v as L.StructType
    return s.fields.length === 0
      ? `(struct ${s.kind.toString()} ())`
      : `(struct ${s.kind.toString()} ${s.fields.map(v => valueToString(col, v, htmlOutput)).join(' ')})`
  } else if (Array.isArray(v)) {
    return v.length === 0
      ? '(vector)'
      : `(vector ${v.map(v => valueToString(col, v, htmlOutput)).join(' ')})`
  } else if (v === null) {
    return 'null'
  } else if (v === undefined) {
    return 'void'
  } else if (typeof v === 'object') {
    // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
    if (Object.hasOwn(v, 'renderAs') && (v as any).renderAs === 'audio') {
      // eslint-disable-next-line @typescript-eslint/no-unsafe-argument, @typescript-eslint/no-unsafe-member-access
      (v as any).storeTag = Runtime.store.add((v as any).data)
      return htmlOutput
        ? `</code><span class="audio">${sanitize(JSON.stringify(v), htmlOutput)}</span><code>`
        : '[object AudioPipeline]'
    // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
    } else if (Object.hasOwn(v, 'renderAs') && (v as any).renderAs === 'drawing') {
      return htmlOutput
        ? `</code><span class="drawing">${sanitize(JSON.stringify(v), htmlOutput)}</span><code>`
        : '[object Drawing]'
    // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
    } else if (Object.hasOwn(v, 'renderAs') && (v as any).renderAs === 'composition') {
      const tag = Runtime.store.add(v)
      return htmlOutput
        ? `</code><span class="composition" id="${tag}"></span><code>`
        : '[object Composition]'
    // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
    } else if ('tagName' in v) {
      // eslint-disable-next-line @typescript-eslint/no-unsafe-argument, @typescript-eslint/no-unsafe-member-access
      const tag = Runtime.store.add(v)
      return htmlOutput
        // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access, @typescript-eslint/restrict-template-expressions
        ? `</code><span class="element" id="${tag}"></span><code>`
        : `[object Element:${(v as Element).tagName}]`
    // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
    } else if (Object.hasOwn(v, 'renderAs') && (v as any).renderAs === 'audio-pipeline') {
      // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
      const tag = Runtime.store.add(v)
      return htmlOutput
        // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access, @typescript-eslint/restrict-template-expressions
        ? `</code><span class="audio-pipeline" id="${tag}"></span><code>`
        : '[object AudioPipeline]'
    // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
    } else if (Object.hasOwn(v, 'renderAs') && (v as any).renderAs === 'reactive-file') {
      const tag = Runtime.store.add(v)
      return htmlOutput
        // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access, @typescript-eslint/restrict-template-expressions
        ? `</code><span class="reactive-file" id="${tag}"></span><code>`
        : '[object ReactiveFile]'
    } else {
      return '[object Object]'
    }
  }
  // eslint-disable-next-line @typescript-eslint/restrict-template-expressions
  throw new ICE('valueToString', `unknown value encountered: ${v}`)
}

export function expToString (col: number, e: L.Exp, htmlOutput: boolean = false): string {
  switch (e.tag) {
    case 'value':
      return valueToString(col, e.value, htmlOutput)
    case 'var':
      return sanitize(e.value, htmlOutput)
    case 'lit':
      return litToString(e.value, htmlOutput)
    case 'call': {
      const allExps = [e.head, ...e.args]
      if (allExps.every(isSimpleExp) && allExps.every(e => nestingDepth(e) <= 4) && e.args.length <= 5) {
        return parens(e.bracket, [e.head].concat(e.args).map(arg => expToString(col, arg, htmlOutput)))
      } else {
        return parens(e.bracket, [
          `${expToString(col, e.head, htmlOutput)}`,
          ...e.args.map(arg => `${indent(col + 2, expToString(col + 2, arg, htmlOutput))}`)
        ], '\n')
      }
    }
    case 'lam': {
      const preamble = `(lambda ${parens(e.bracket, e.args.map(n => n.value))}`
      if (isSimpleExp(e.body)) {
        return [preamble, `${expToString(col, e.body, htmlOutput)})`].join(' ')
      } else {
        return [preamble, `${indent(col + 2, expToString(col + 2, e.body, htmlOutput))})`].join('\n')
      }
    }
    case 'if': {
      return parens(e.bracket, [
        `if ${expToString(col, e.e1, htmlOutput)}`,
        `${indent(col + 2, expToString(col + 2, e.e2, htmlOutput))}`,
        `${indent(col + 2, expToString(col + 2, e.e3, htmlOutput))}`
      ], '\n')
    }
    case 'nil':
      return 'null'
    case 'pair':
      return e.isList
        ? parens(e.bracket, ['list'].concat(L.unsafeListToArray(e).map(arg => expToString(col, arg, htmlOutput))))
        : parens(e.bracket, ['cons', expToString(col, e.e1, htmlOutput), expToString(col, e.e2, htmlOutput)])
    case 'let': {
      const preamble = `${e.kind} `
      // N.B., this bracket is difficult to factor out using paren...
      const firstBinding = `${indent(col + 2, `([${e.bindings[0][0].value} ${expToString(col + 2 + e.bindings[0][0].value.length + 1, e.bindings[0][1], htmlOutput)}]`)}`
      const bindings = e.bindings.length === 1
        ? firstBinding + ')'
        : [firstBinding, ...e.bindings.slice(1).map(b => `${indent(col + 2 + 1, `[${b[0].value} ${expToString(col + 2 + 1 + b[0].value.length + 1, b[1], htmlOutput)}]`)}`)].join('\n') + ')'
      const body = `${indent(col + 2, `${expToString(col + 2, e.body, htmlOutput)}`)}`
      return parens(e.bracket, [preamble, bindings, body], '\n')
    }
    case 'cond': {
      const preamble = 'cond '
      const bindings = e.branches.map(b => indent(col + 2, `[${expToString(col + 2, b[0], htmlOutput)} ${expToString(col + 2, b[1], htmlOutput)}]`))
      return parens(e.bracket, [preamble, ...bindings], '\n')
    }
    case 'and':
      return parens(e.bracket, ['and', ...e.args.map(arg => expToString(col + 2, arg, htmlOutput))])
    case 'or':
      return parens(e.bracket, ['or', ...e.args.map(arg => expToString(col + 2, arg, htmlOutput))])
    case 'match': {
      const bindings = e.branches.map(b => indent(col + 2, `[${patToString(b[0], htmlOutput)} ${expToString(col + 2, b[1], htmlOutput)}]`))
      return parens(e.bracket, [`match ${expToString(col, e.scrutinee, htmlOutput)}`, ...bindings], '\n')
    }
    case 'begin': {
      return parens(e.bracket, ['begin', ...e.exps.map(e => expToString(0, e, htmlOutput))])
    }
  }
}

export function effectToString (col: number, effect: L.SEffect, outputBindings: boolean = false, htmlOutput: boolean = false): string {
  switch (effect.tag) {
    case 'error': return effect.errors.map(err => detailsToCompleteString(err)).join('\n')
    case 'binding': return outputBindings ? `[[${sanitize(effect.name, htmlOutput)} bound]]` : ''
    case 'testresult': {
      if (effect.passed) {
        return `[[ Test "${sanitize(effect.desc, htmlOutput)}" passed! ]]`
      } else {
        const msg: string = effect.reason
          ? effect.reason
          : `  Expected: ${expToString(col, effect.expected!, htmlOutput)}\n  Actual: ${expToString(col, effect.actual!, htmlOutput)}`
        return `[[ Test "${sanitize(effect.desc, htmlOutput)}" failed!\n${msg}\n]]`
      }
    }
    case 'value': return effect.output ? effect.output : 'void'
    case 'imported': return outputBindings ? `[[${sanitize(effect.source, htmlOutput)} imported]]` : ''
  }
}

export function stmtToString (col: number, stmt: L.Stmt, outputBindings: boolean = false, htmlOutput: boolean = false): string {
  switch (stmt.tag) {
    case 'define': {
      const preamble = `(define ${sanitize(stmt.name.value, htmlOutput)}`
      return isSimpleExp(stmt.value)
        ? `${preamble} ${expToString(col, stmt.value, htmlOutput)})`
        : `${preamble}\n${indent(col + 2, expToString(col + 2, stmt.value, htmlOutput))})`
    }
    case 'struct': return `(struct ${sanitize(stmt.id.value, htmlOutput)} (${stmt.fields.map(f => f.value).join(' ')}))`
    case 'testcase':
      return `(test-case ${expToString(col, stmt.desc, htmlOutput)} ${expToString(col, stmt.comp, htmlOutput)}\n${indent(col + 2, expToString(col + 2, stmt.expected, htmlOutput))}\n${indent(col + 2, expToString(col + 2, stmt.actual, htmlOutput))})`
    case 'exp': return expToString(col, stmt.value, htmlOutput)
    case 'import': return `(import ${sanitize(stmt.source, htmlOutput)})`
    case 'error': return effectToString(col, stmt, outputBindings, htmlOutput)
    case 'binding': return effectToString(col, stmt, outputBindings, htmlOutput)
    case 'testresult': return effectToString(col, stmt, outputBindings, htmlOutput)
    case 'value': return effectToString(col, stmt, outputBindings, htmlOutput)
    case 'imported': return effectToString(col, stmt, outputBindings, htmlOutput)
  }
}

export function progToString (
  col: number, prog: L.Program, outputBindings: boolean = false,
  htmlOutput: boolean = false, lineSep: string = '\n'): string {
  return prog
    .map(s => stmtToString(col, s, outputBindings, htmlOutput))
    .filter(s => s.length > 0)
    .map(s => htmlOutput ? `<code>${s}</code>` : s)
    .join(lineSep)
}
