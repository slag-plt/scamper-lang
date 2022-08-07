import { ICE } from './result.js'

type Template = (...args: any) => string
type Messages = Map<string, Template>

const templates: Messages = new Map([
  ['error-arity', (vs) => `${vs[0]} expects ${vs[1]} arguments but ${vs[2]} arguments were given`],
  ['error-arity-atleast', (vs) => `${vs[0]} expects at least ${vs[1]} arguments but ${vs[2]} arguments were given`],
  ['error-cond-no-branch-applies', (_) => 'No guard of the conditional expression evaluated to #t'],
  ['error-empty-app', (_) => 'Function application cannot be empty'],
  ['error-eof', (_) => 'Unexpected end of source code encountered'],
  ['error-import-not-found', (vs) => `Import ${vs[0]} not found`],
  ['error-index-string', (vs) => `${vs[0]} is not a valid index into string "${vs[1]}"`],
  ['error-invalid-string-literal', (_) => 'Invalid string literal encountered'],
  ['error-precondition-not-met', (vs) => `${vs[0]} expects that the argument obeys the following property: ${vs[1]} but ${vs[2]} was given.`],
  ['error-missing-parens', (_) => 'Missing closing parentheses'],
  ['error-unmatched-parens', (_) => 'Unmatched parentheses encountered'],
  ['error-reserved-word', (_) => 'Cannot use reserved word as a variable name'],
  ['error-type-expected', (vs) => `A ${vs[0]} was expected, but a ${vs[1]} was found`],
  ['error-type-expected-fun', (vs) => `${vs[0]} expected an ${vs[1]} but a ${vs[2]} was given`],
  ['error-type-expected-call', (vs) => `A function was expected as the first argument to a function call, but a ${vs[0]} was given`],
  ['error-type-expected-cond', (vs) => 'A boolean was expected as the guard of a conditional, but a ${vs[0]) was given'],
  ['error-var-binding', (_) => 'A variable was expected in the first position of the binding'],
  ['error-var-undef', (vs) => `Variable ${vs[0]} is not defined`],
  ['error-var-shadowed', (vs) => `Variable ${vs[0]} has already been defined`],
  ['phase-lexer', (_) => 'Lexer'],
  ['phase-parser', (_) => 'Parser'],
  ['phase-runtime', (_) => 'Runtime'],
  ['phase-scope', (_) => 'Scope']
])

export function msg (id: string, ...args: any): string {
  if (templates.has(id)) {
    return templates.get(id)!(args)
  } else {
    throw new ICE('msg', `Identifier not found: ${id}`)
  }
}
