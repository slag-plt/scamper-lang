import * as L from './lang.js'
import * as P from './pretty.js'
import { ICE } from './result.js'

// function compileIdentifier()

function compileLit (l: L.Lit): string {
  switch (l.tag) {
    case 'bool':
      return l.value ? 'true' : 'false'
    case 'num':
      return l.value.toString()
    case 'char':
      return `{ _scamperTag: 'char', value: "${l.value}" }`
    case 'str':
      // TODO: need to escape this string
      return `"${l.value}"`
  }
}

function compileExp (e: L.Exp): string {
  switch (e.tag) {
    case 'value':
      throw new ICE('compileExp', `values cannot be compiled: ${P.expToString(0, e, false)}`)
    case 'var':
      // TODO: need to "escape" these identifiers
      return e.value
    case 'lit':
      return compileLit(e.value)
    case 'call':
      return `(${compileExp(e.head)})(${e.args.map(compileExp).join(', ')})`
    case 'lam':
      return `(${e.args.join(', ')}) => (${compileExp(e.body)})`
    case 'if':
      return `(${compileExp(e.e1)}) ? (${compileExp(e.e2)}) : (${compileExp(e.e3)})`
    case 'nil':
      return 'null'
    case 'pair':
      return `{ _scamperTag: 'pair', fst: ${compileExp(e.e1)}, snd: ${compileExp(e.e2)} }`
    case 'let': {
      let result = compileExp(e.body)
      for (let i = e.bindings.length - 1; i >= 0; i--) {
        result = `(${(e.bindings[i][0].value)} => (${result}))(${compileExp(e.bindings[i][1])})`
      }
      return result
    }
    case 'cond': {
      let result = 'throw new Error("No matching branch in cond expression")'
      for (let i = e.branches.length - 1; i >= 0; i--) {
        result = `(${compileExp(e.branches[i][0])}) ? (${compileExp(e.branches[i][1])}) : (${result})`
      }
      return result
    }
    case 'and':
      return e.args.map(compileExp).join(' && ')
    case 'or':
      return e.args.map(compileExp).join(' || ')
    case 'begin':
      return `(${e.exps.map(compileExp).join(', ')})`
    case 'match':
      throw new ICE('compileExp', 'match expression not yet supported')
  }
}

export function compileStmt (s: L.Stmt): string {
  switch (s.tag) {
    case 'import':
      throw new ICE('compileStmt', 'import statement not yet supported')
    case 'define':
      return `const ${s.name.value} = ${compileExp(s.value)}`
    case 'exp':
      return `console.log(${compileExp(s.value)})`
    case 'struct':
      throw new ICE('compileStmt', 'struct statement not yet supported')
    case 'testcase':
      throw new ICE('compileStmt', 'testcase statement not yet supported')
    default:
      throw new ICE('compileStmt', 'effects cannot be compiled')
  }
}
