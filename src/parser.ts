import { eand, ebool, ecall, echar, econd, eif, elam, elet, enil, enumber, eor, estr, evar, Exp, Name, name, Program, sdefine, sexp, simport, sstruct, Stmt } from './lang.js'
import { error, join, ok, Result, rethrow } from './result.js'
import { Atom, Sexp, sexpToString, stringToSexp, stringToSexps, Token } from './sexp.js'
import { msg } from './messages.js'

const reservedWords = [
  'and',
  'cond',
  'define',
  'if',
  'import',
  'lambda',
  'or',
  'struct'
]

function parserError <T> (message: string, s?: Sexp, hint?: string): Result<T> {
  return error(msg('phase-parser'), message, s?.range, s ? sexpToString(s) : undefined, hint)
}

function checkDuplicateNames (names: Name[], s: Sexp): Result<Name[]> {
  const soFar: string[] = []
  for (const name of names) {
    if (soFar.includes(name.value)) {
      return parserError(msg('error-duplicate-name', name.value), s)
    } else {
      soFar.push(name.value)
    }
  }
  return ok(names)
} 

function sexpToUniqueStringList (s: Sexp): Result<Name[]> {
  switch (s.tag) {
    case 'atom':
      return parserError(msg('error-type-expected', 'list', 'identifier'), s)
    case 'slist':
      return join<Name>(s.list.map((x) => x.tag === 'atom'
        ? ok(name(x.single, x.range))
        : parserError(msg('error-type-expected', 'identifier', 'list'), x)))
            .andThen(names => checkDuplicateNames(names, s))
  }
}

function sexpToBinding (s: Sexp): Result<[Name, Exp]> {
  switch (s.tag) {
    case 'atom':
      return parserError(msg('error-type-expected', 'binding', 'identifier'), s)
    case 'slist':
      return s.list.length !== 2
        ? parserError(msg('error-type-expected', 'binding', 'non-binding'), s)
        : s.list[0].tag !== 'atom'
          ? parserError(msg('error-var-binding'), s)
          : sexpToExp(s.list[1]).andThen((e:Exp) =>
              ok([name((s.list[0] as Atom).single, (s.list[0] as Atom).range), e] as [Name, Exp]))
  }
}

function sexpToBindings (s: Sexp): Result<[Name, Exp][]> {
  switch (s.tag) {
    case 'atom':
      return parserError(msg('error-type-expected', 'binding list', 'identifier'), s)
    case 'slist': {
      const result: [Name, Exp][] = new Array(s.list.length)
      for (let i = 0; i < s.list.length; i++) {
        const r = sexpToBinding(s.list[i])
        switch (r.tag) {
          case 'error':
            return rethrow(r)
          case 'ok':
            result[i] = r.value
        }
      }
      return ok(result)
    }
  }
}

function sexpToBranch (s: Sexp): Result<[Exp, Exp]> {
  switch (s.tag) {
    case 'atom':
      return parserError(msg('error-type-expected', 'branch', 'identifier'), s)
    case 'slist':
      if (s.list.length !== 2) {
        return parserError(msg('error-type-expected', 'branch', 'non-branch'), s)
      } else {
        return sexpToExp(s.list[0]).andThen(guard =>
          sexpToExp(s.list[1]).andThen(body => ok([guard, body])))
      }
  }
}

function tryParseString (s: string): string | undefined {
  if (s.length < 2 || !s.startsWith('"') || !s.endsWith('"')) {
    return undefined
  } else {
    // N.B., escape codes for strings are expanded in the lexer/sexp.ts.
    return s.slice(1, s.length - 1)
  }
}

const intRegex = /^[+-]?\d+$/
const floatRegex = /^[+-]?(\d+|(\d*\.\d+)|(\d+\.\d*))([eE][+-]?\d+)?$/

export const namedCharValues = new Map([
  ['alarm', String.fromCharCode(9)], 
  ['backspace', String.fromCharCode(7)],
  ['delete', String.fromCharCode(126)],
  ['escape', String.fromCharCode(26)],
  ['newline', String.fromCharCode(9)],
  ['null', String.fromCharCode(-1)],
  ['return', String.fromCharCode(12)],
  ['space', ' '],
  ['tab', String.fromCharCode(8)]
])

function sexpToExp (s: Sexp): Result<Exp> {
  switch (s.tag) {
    case 'atom': {
      if (intRegex.test(s.single)) {
        return ok(enumber(s.range, parseInt(s.single, 10)))
      } else if (floatRegex.test(s.single)) {
        return ok(enumber(s.range, parseFloat(s.single)))
      } else if (s.single.startsWith('"')) {
        const result = tryParseString(s.single)
        // N.B., '' is false in Javascript, so need an explicit undefined check.
        return result !== undefined
          ? ok(estr(s.range, result))
          : parserError(msg('error-invalid-string-literal'), s)
      } else if (s.single === 'null') {
        return ok(enil(s.range))
      } else if (s.single === '#t' || s.single === '#true') {
        return ok(ebool(s.range, true))
      } else if (s.single === '#f' || s.single === '#false') {
        return ok(ebool(s.range, false))
      } else if (s.single.startsWith('#\\')) {
        const result = s.single.slice(2)
        if (result.length === 1) {
          return ok(echar(s.range, result))
        } else {
          // N.B., the char is a named character. We already checked that
          // the named character was valid in the lexer, so we can simply
          // translate the name into the appropriate character value.
          return ok(echar(s.range, namedCharValues.get(result)!))
        }
      } else if (reservedWords.includes(s.single)) {
        return parserError(msg('error-reserved-word'), s)
      } else {
        return ok(evar(s.range, s.single))
      }
    }

    case 'slist':
      if (s.list.length === 0) {
        return parserError(msg('error-empty-app'))
      } else {
        const head = s.list[0]
        const args = s.list.slice(1)
        switch (head.tag) {
          case 'atom':
            switch (head.single) {
              case 'lambda':
                return args.length !== 2
                  ? parserError(msg('error-arity', 'lambda', 2, args.length), s)
                  : sexpToUniqueStringList(args[0]).andThen(xs =>
                    sexpToExp(args[1]).andThen(body =>
                      ok(elam(s.range, xs, body, s.bracket))))
              case 'if':
                return args.length !== 3
                  ? parserError(msg('error-arity', 'if', 3, args.length), s)
                  : sexpToExp(args[0]).andThen(e1 =>
                    sexpToExp(args[1]).andThen(e2 =>
                      sexpToExp(args[2]).andThen(e3 =>
                        ok(eif(s.range, e1, e2, e3, s.bracket)))))
              case 'let':
                return args.length !== 2
                  ? parserError(msg('error-arity', 'let', 2, args.length), s)
                  : sexpToBindings(args[0]).andThen(bindings =>
                    sexpToExp(args[1]).andThen(body =>
                      ok(elet(s.range, bindings, body, s.bracket))))
              case 'cond':
                return args.length === 0
                  ? parserError(msg('error-arity-atleast', 'cond', '1', args.length), s)
                  : join(args.map(sexpToBranch)).andThen(branches =>
                    ok(econd(s.range, branches, s.bracket)))
              case 'and':
                return join(args.map(sexpToExp)).andThen(es => ok(eand(s.range, es, s.bracket)))
              case 'or':
                return join(args.map(sexpToExp)).andThen(es => ok(eor(s.range, es, s.bracket)))
              default:
                // NOTE: applications whose head are not keywords are assumed to be applications.
                return join(args.map(sexpToExp)).andThen(es => ok(ecall(s.range, evar(head.range, head.single), es, s.bracket)))
            }
          case 'slist':
            return sexpToExp(head).andThen(e =>
              join(args.map(sexpToExp)).andThen(es =>
                ok(ecall(s.range, e, es, s.bracket))))
        }
      }
  }
}

function parseExp (src: string): Result<Exp> {
  return stringToSexp(src).andThen(sexp => sexpToExp(sexp))
}

function sexpToStmt (s: Sexp): Result<Stmt> {
  switch (s.tag) {
    case 'atom':
      return sexpToExp(s).andThen(e => ok(sexp(e)))
    case 'slist':
      if (s.list[0].tag === 'atom' && s.list[0].single === 'define') {
        const args = s.list.slice(1)
        return args.length !== 2
          ? parserError(`Define expects 2 arguments, ${args.length} given`, s)
          : args[0].tag !== 'atom'
            ? parserError('Define expects a variable as the first argument', s)
            : sexpToExp(args[1]).andThen(e => ok(sdefine(name((args[0] as Atom).single, (args[0] as Atom).range), e)))
      } else if (s.list[0].tag === 'atom' && s.list[0].single === 'import') {
        const args = s.list.slice(1)
        if (args.length !== 1) {
          return parserError(msg('error-arity', 'import', 1, args.length), s)
        } else {
          const source = args[0]
          return source.tag !== 'atom'
            ? parserError(msg('error-type-expected', 'module name', source.tag), s)
            : ok(simport(s.range, source.single))
        }
      } else if(s.list[0].tag === 'atom' && s.list[0].single === 'struct') {
        const args = s.list.slice(1)
        if (args.length !== 2) {
          return parserError(msg('error-arity', 'struct', 2, args.length), s)
        } else if (args[0].tag !== 'atom') {
          return parserError(msg('error-type-expected', 'struct name', args[0].tag), s)
        } else {
          return sexpToUniqueStringList(args[1]).andThen(fields =>
            ok(sstruct(name((args[0] as Atom).single, args[0].range), fields)))
        }
      } else {
        return sexpToExp(s).andThen(e => ok(sexp(e)))
      }
  }
}

function sexpsToProgram (ss: Sexp[]): Result<Program> {
  return join(ss.map(sexpToStmt)).andThen(statements => ok({
    statements
  }))
}

function parseProgram (src: string): Result<Program> {
  return stringToSexps(src).andThen(ss => sexpsToProgram(ss))
}

export {
  parseExp, parseProgram, sexpsToProgram
}
