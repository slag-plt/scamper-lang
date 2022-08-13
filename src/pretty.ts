import * as L from './lang.js'

function litToString (l: L.Lit): string {
  switch (l.tag) {
    case 'bool': return l.value ? '#t' : '#f'
    case 'num': return l.value.toString()
    case 'char': return `#${l.value}`
    case 'str': return `"${l.value}"`
  }
}

function isSimpleExp (e: L.Exp): boolean {
  switch (e.tag) {
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
    case 'obj': return true
    case 'prim': return true
  }
}

function parens(ss: string[]): string {
  return `(${ss.join(' ')})`
}

function indent(col: number, s: string): string {
  return `${' '.repeat(col)}${s}`
}

export function expToString (col: number, e: L.Exp): string {
  switch (e.tag) {
    case 'var':
      return e.value
    case 'lit':
      return litToString(e.value)
    case 'call':
      return parens([e.head].concat(e.args).map(arg => expToString(col, arg)))
    case 'lam': {
      const preamble = `(lambda ${parens(e.args.map(n => n.value))}`
      if (isSimpleExp(e.body)) {
        return [preamble, `${expToString(col, e.body)})`].join(' ')
      } else {
        return [preamble, `${indent(col + 2, expToString(col + 2, e.body))}`].join('\n')
      }
    }
    case 'if': {
      return [
        `(if ${expToString(col, e.e1)}`, 
        `${indent(col + 2, expToString(col + 2, e.e2))}`,
        `${indent(col + 2, expToString(col + 2, e.e3))})`,
      ].join('\n')
    }
    case 'nil':
      return 'null'
    case 'pair':
      return L.isList(e)
        ? parens(['list'].concat(L.unsafeListToArray(e).map(arg => expToString(col, arg))))
        : parens(['cons', expToString(col, e.e1), expToString(col, e.e2)])
    case 'let': {
      const preamble = '(let '
      const firstBinding = `${indent(col + 2, `([${e.bindings[0][0]} ${expToString(col + 2 + e.bindings[0][0].value.length + 1, e.bindings[0][1])}]`)}`
      const bindings = e.bindings.length == 1
        ? firstBinding + ')'
        : [firstBinding, ...e.bindings.map(b => `${indent(col + 2 + 1, `[${b[0]} ${expToString(col + 2 + 1 + b[0].value.length + 1, b[1])}]`)}`)].join('\n') + ')'
      const body = `${indent(col + 2, `(${expToString(col + 2, e.body)})`)}`
      return [preamble, bindings, body].join('\n') + ')'
    }
    case 'cond': {
      const preamble = '(cond '
      const bindings = e.branches.map(b => indent(col + 2, `[${expToString(col + 2, b[0])} ${expToString(col + 2, b[1])}]`))
      return [preamble, ...bindings].join('\n') + ')'
    }
    case 'and':
      return parens(['and', ...e.args.map(arg => expToString(col + 2, arg))])
    case 'or':
      return parens(['or', ...e.args.map(arg => expToString(col + 2, arg))])
    case 'obj':
      return `[object ${e.kind}]`
    case 'prim':
      return `[prim ${e.prim.name}]`
  }
}

export function stmtToString(col: number, stmt: L.Stmt, outputBindings: boolean = false): string {
  switch (stmt.tag) {
    case 'define': {
      const preamble = `(define ${stmt.name.value}`
      return isSimpleExp(stmt.value)
        ? `${preamble} ${expToString(col, stmt.value)}`
        : `${preamble}\n${indent(col + 2, expToString(col + 2, stmt.value))}`
    }
    case 'exp': return expToString(col, stmt.value)
    case 'import': return `(import ${stmt.source})`
    case 'error': return stmt.errors.map(err => `<<error: ${err.message}>>`).join('\n')
    case 'binding': return outputBindings ? `<<${stmt.name} bound>>` : ''
    case 'value': return expToString(col, stmt.value)
    case 'imported': return outputBindings ? `<<[${stmt.source} imported>>` : ''
  }
}

export function progToString(col: number, prog: L.Program, outputBindings: boolean = false): string {
  return prog.statements.map(s => stmtToString(col, s, outputBindings)).filter(s => s.length > 0).join('\n\n')
}