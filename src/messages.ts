import { ICE } from './result.js'

type Template = (...args: any) => string
type Messages = Map<string, Template>

const templates: Messages = new Map([
  ['error-arity', (vs) => `${vs[0]} expects ${vs[1]} arguments but ${vs[2]} arguments were given`],
  ['error-arity-atleast', (vs) => `${vs[0]} expects at least ${vs[1]} arguments but ${vs[2]} arguments were given`],
  ['error-cond-no-branch-applies', (_) => 'No guard of the conditional expression evaluated to #t'],
  ['error-duplicate-name', (vs) => `Duplicate name ${vs[0]} given in definition.`],
  ['error-empty-app', (_) => 'Function application cannot be empty'],
  ['error-eof', (_) => 'Unexpected end of source code encountered'],
  ['error-eof-string', (_) => 'Unexpected end of source code encountered while parsing a string literal'],
  ['error-eof-block-comment', (_) => 'Unexpected end of source code encountered while parsing a block comment'],
  ['error-file-not-found', (vs) => `File ${vs[0]} not found`],
  ['error-hole', (_) => 'A hole was encountered!'],
  ['error-import-not-found', (vs) => `Import ${vs[0]} not found`],
  ['error-index-string', (vs) => `${vs[0]} is not a valid index into string "${vs[1]}"`],
  ['error-invalid-char-constant', (_) => 'Invalid character constant encountered'],
  ['error-invalid-string-literal', (_) => 'Invalid string literal encountered'],
  ['error-runtime-parsing', (vs) => `${vs[0]} expects that its input string ${vs[1]} is a valid ${vs[2]} but it is not.`],
  ['error-precondition-not-met', (vs) => `${vs[0]} expects that argument ${vs[1]} obeys the following property: ${vs[2]}, but ${vs[3]} was given.`],
  ['error-missing-parens', (_) => 'Missing closing parentheses'],
  ['error-unmatched-parens', (_) => 'Unmatched parentheses encountered'],
  ['error-unrecognized-escape', (vs) => `Unrecognized escape sequence: ${vs[0]}`],
  ['error-reserved-word', (_) => 'Cannot use reserved word as a variable name'],
  ['error-runtime', (vs) => `A runtime error was encounter: ${vs[0]}`],
  ['error-type-expected', (vs) => `A ${vs[0]} was expected, but a ${vs[1]} was found`],
  ['error-type-expected-fun', (vs) => `${vs[0]} expected an ${vs[1]} in position ${vs[2]} but a ${vs[3]} was given`],
  ['error-type-expected-call', (vs) => `A function was expected as the first argument to a function call, but a ${vs[0]} was given`],
  ['error-type-expected-cond', (vs) => `A boolean was expected as the guard of a conditional, but ${vs[0]} was given`],
  ['error-type-filter-bool', (vs) => `Filter's predicate must return a boolean, but a ${vs[0]} was returned instead.`],
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
