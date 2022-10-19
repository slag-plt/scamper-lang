/* eslint-disable @typescript-eslint/no-unsafe-call */
/* eslint-disable @typescript-eslint/no-unsafe-assignment */
/* eslint-disable @typescript-eslint/restrict-template-expressions */
/* eslint-disable @typescript-eslint/no-unsafe-member-access */
import * as L from './lang.js'
import { detailsToCompleteString, ICE } from './result.js'
import * as P from './parser.js'

const namedCharTable = new Map(Array.from(P.namedCharValues.entries()).map(([name, value]) => [value, name]))

function charToName (s: string): string {
  return namedCharTable.get(s) ?? s
}

function litToString (l: L.Lit): string {
  switch (l.tag) {
    case 'bool': return l.value ? '#t' : '#f'
    case 'num': return l.value.toString()
    case 'char': return `#\\${charToName(l.value)}`
    case 'str': return `"${l.value}"`
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
    case 'struct': return true
    case 'prim': return true
    case 'match': return false
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
    case 'struct': return 0
    case 'prim': return 0
    case 'match': return 0 // TODO
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

function patToString (p: L.Pat): string {
  switch (p.tag) {
    case 'var': return p.id
    case 'wild': return '_'
    case 'null': return 'null'
    case 'lit': return litToString(p.lit)
    case 'ctor': return parens('(', [p.head, ...p.args.map(patToString)])
  }
}

export function valueToString (col: number, v: L.Value, htmlOutput: boolean = false): string {
  if (typeof v === 'boolean') {
    return v ? '#t' : '#f'
  } else if (typeof v === 'number') {
    return v.toString()
  } else if (typeof v === 'string') {
    return `"${v}"`
  } else if (L.isTaggedObject(v)) {
    if (v.tag === 'char') {
      return `#\\${v.value}`
    } else if (v.tag === 'lambda' || v.tag === 'prim') {
      return '[object Function]'
    } else if (v.tag === 'pair') {
      return L.valueIsList(v)
        ? `(list ${L.valueListToArray_(v).map(L.valueToString).join(' ')})`
        : `(pair ${L.valueToString(v.fst)} ${L.valueToString(v.snd)})`
    } else if (v.tag === 'struct') {
      return `(struct ${v.kind} ${[...v.fields.values()].map(v => valueToString(col, v, htmlOutput)).join(' ')})`
    } else {
      return '[object Object]'
    }
  } else if (Array.isArray(v)) {
    throw new ICE('valueToString', 'vector not yet implemented')
  } else {
    throw new ICE('valueToString', `unrecognized value: ${v.toString()}`)
  }
}

export function expToString (col: number, e: L.Exp, htmlOutput: boolean = false): string {
  switch (e.tag) {
    case 'var':
      return e.value
    case 'lit':
      return litToString(e.value)
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
      return L.isList(e)
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
    case 'struct':
      if (e.kind === 'Drawing') {
        return htmlOutput
          ? `</code><span class="drawing">${JSON.stringify(e.obj)}</span><code>`
          : '[object Drawing]'
      } else if (e.kind === 'Composition') {
        return htmlOutput
          ? `</code><span class="composition">${JSON.stringify(e.obj)}</span><code>`
          : '[object Composition]'
      } else {
        // eslint-disable-next-line @typescript-eslint/no-unsafe-argument, @typescript-eslint/no-unsafe-member-access
        return parens('(', [`struct ${e.kind}`, ...Object.keys(e.obj).map(k => expToString(col, (e.obj as any)[k], htmlOutput))])
      }
    case 'prim':
      return `[prim ${e.prim.name}]`
    case 'match': {
      const bindings = e.branches.map(b => indent(col + 2, `[${patToString(b[0])} ${expToString(col + 2, b[1], htmlOutput)}]`))
      return parens(e.bracket, [`match ${expToString(col, e.scrutinee, htmlOutput)}`, ...bindings], '\n')
    }
  }
}

export function stmtToString (col: number, stmt: L.Stmt, outputBindings: boolean = false, htmlOutput: boolean = false): string {
  switch (stmt.tag) {
    case 'define': {
      const preamble = `(define ${stmt.name.value}`
      return isSimpleExp(stmt.value)
        ? `${preamble} ${expToString(col, stmt.value, htmlOutput)})`
        : `${preamble}\n${indent(col + 2, expToString(col + 2, stmt.value, htmlOutput))})`
    }
    case 'struct': return `(struct ${stmt.id.value} (${stmt.fields.map(f => f.value).join(' ')}))`
    case 'testcase':
      return `(test-case ${expToString(col, stmt.desc, htmlOutput)} ${expToString(col, stmt.comp, htmlOutput)}\n${indent(col + 2, expToString(col + 2, stmt.expected, htmlOutput))}\n${indent(col + 2, expToString(col + 2, stmt.actual, htmlOutput))})`
    case 'exp': return expToString(col, stmt.value, htmlOutput)
    case 'import': return `(import ${stmt.source})`
    case 'error': return stmt.errors.map(err => detailsToCompleteString(err)).join('\n')
    case 'binding': return outputBindings ? `[[${stmt.name} bound]]` : ''
    case 'testresult': {
      if (stmt.passed) {
        return `[[ Test "${stmt.desc}" passed! ]]`
      } else {
        const msg: string = stmt.reason
          ? stmt.reason
          : `  Expected: ${expToString(col, stmt.expected!, htmlOutput)}\n  Actual: ${expToString(col, stmt.actual!, htmlOutput)}`
        return `[[ Test "${stmt.desc}" failed!\n${msg}\n]]`
      }
    }
    case 'value': return expToString(col, stmt.value, htmlOutput)
    case 'imported': return outputBindings ? `[[${stmt.source} imported]]` : ''
  }
}

export function progToString (
  col: number, prog: L.Program, outputBindings: boolean = false,
  htmlOutput: boolean = false, lineSep: string = '\n'): string {
  return prog.statements
    .map(s => stmtToString(col, s, outputBindings, htmlOutput))
    .filter(s => s.length > 0)
    .map(s => htmlOutput ? `<code>${s}</code>` : s)
    .join(lineSep)
}
