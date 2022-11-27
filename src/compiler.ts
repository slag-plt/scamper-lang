import * as L from './lang.js'
import * as P from './pretty.js'
import { ICE } from './result.js'

export const reservedWords: string[] = [
  'break', 'case', 'catch', 'class', 'const', 'continue', 'debugger',
  'default', 'delete', 'do', 'else', 'export', 'extends', 'false', 'finally',
  'for', 'function', 'if', 'import', 'in', 'instanceof', 'new', 'null',
  'return', 'super', 'switch', 'this', 'throw', 'true', 'try',
  'typeof', 'var', 'void', 'while', 'with'
]

export const specialCharMap: Map<string, string> = new Map([
  ['!', 'BANG'],
  ['@', 'AT'],
  ['#', 'HASH'],
  ['%', 'PERCENT'],
  ['^', 'CARET'],
  ['&', 'AMPERSAND'],
  ['*', 'STAR'],
  ['-', 'DASH'],
  ['=', 'EQUALS'],
  ['+', 'PLUS'],
  ['|', 'PIPE'],
  ['\\', 'BSLASH'],
  [':', 'COLON'],
  [';', 'SEMICOLON'],
  ['?', 'QUESTION'],
  ['/', 'FSLASH'],
  [',', 'COMMA'],
  ['.', 'DOT']
])

// When the compiler mangles a name, we use this prefix to avoid collisions with
// other generated names. Note that because of Scheme's permissive identifier
// syntax, it is difficult to guarantee that some mangled name does not conflict
// with a user-defined name. Therefore, we pick an "obvious" prefix as our
// mangle strategy, relying on the user to avoid naming conflicts when possible.
const manglePrefix: string = '__'

function compileIdentifier (s: string): string {
  // Immediately, mangle reserved words.
  if (reservedWords.includes(s)) {
    return `${manglePrefix}${s}`
  }
  // Otherwise, build up a valid Javascript identifier character-by-character.
  let result = ''

  // If an identifier starts with a number, mangle it to make it valid.
  let mangled = s.match(/^\d/) !== null
  for (let i = 0; i < s.length; i++) {
    // TODO: should make sure ch is a valid unicode letter or number.
    const ch = s.at(i)!
    if (specialCharMap.has(ch)) {
      mangled = true
      result += specialCharMap.get(ch)!
    } else {
      result += ch
    }
  }
  return mangled ? `${manglePrefix}result` : result
}

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
      return compileIdentifier(e.value)
    case 'lit':
      return compileLit(e.value)
    case 'call':
      return `(${compileExp(e.head)})(${e.args.map(compileExp).join(', ')})`
    case 'lam':
      return `(${e.args.map(n => compileIdentifier(n.value)).join(', ')}) => (${compileExp(e.body)})`
    case 'if':
      return `(${compileExp(e.e1)}) ? (${compileExp(e.e2)}) : (${compileExp(e.e3)})`
    case 'nil':
      return 'null'
    case 'pair':
      return `{ _scamperTag: 'pair', fst: ${compileExp(e.e1)}, snd: ${compileExp(e.e2)} }`
    case 'let': {
      let result = compileExp(e.body)
      for (let i = e.bindings.length - 1; i >= 0; i--) {
        result = `(${(compileIdentifier(e.bindings[i][0].value))} => (${result}))(${compileExp(e.bindings[i][1])})`
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
      return [
        `import * as _${s.source}_MODULE from "./${s.source}.js"`,
        `Object.entries(_${s.source}_MODULE).forEach(([n, v]) => global[n] = v);`
      ].join('\n')
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

export function compileProgram (s: L.Program): string {
  return s.map(compileStmt).join('\n\n')
}
