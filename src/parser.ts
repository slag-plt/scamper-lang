import { eand, ebool, ecall, econd, eif, elam, elet, enil, enumber, eor, estr, evar, Exp, Name, name, Program, sdefine, sexp, Stmt } from './lang.js'
import { error, join, ok, Result, rethrow } from './result.js'
import { Atom, Sexp, sexpToString, SList, stringToSexp, stringToSexps } from './sexp.js'
import { msg } from './messages.js'

const reservedWords = [
  'and',
  'cond',
  'define',
  'if',
  'import',
  'lambda',
  'or'
]

function parserError <T> (message: string, s?: Sexp, hint?: string): Result<T> {
  return error(msg('phase-parser'), message, s?.range, s ? sexpToString(s) : undefined, hint)
}

function sexpToStringList (s: Sexp): Result<Name[]> {
  switch (s.tag) {
    case 'atom':
      return parserError(msg('error-type-expected', 'list', 'identifier'), s)
    case 'slist':
      return join(s.list.map((x) => x.tag === 'atom'
        ? ok(name(x.single, x.range))
        : parserError(msg('error-type-expected', 'identifier', 'list'), x)))
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
    // TODO: for now, we only support simple string literals with no escape codes.
    const src = s.slice(1, s.length - 1)
    return src
  }
}

function sexpToExp (s: Sexp): Result<Exp> {
  switch (s.tag) {
    case 'atom': {
      // TODO: check to see if the atom is a number, character, boolean, or string
      // TODO: number parsing is complicated, this is a quick hack to move forward
      const asInt = parseInt(s.single, 10)
      if (!isNaN(asInt)) {
        return ok(enumber(s.range, asInt))
      } else if (s.single.startsWith('"')) {
        const result = tryParseString(s.single)
        return result
          ? ok(estr(s.range, result))
          : parserError(msg('error-invalid-string-literal'), s)
      } else if (s.single === 'null') {
        return ok(enil(s.range))
      } else if (s.single === '#t' || s.single === '#true') {
        return ok(ebool(s.range, true))
      } else if (s.single === '#f' || s.single === '#false') {
        return ok(ebool(s.range, false))
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
                  : sexpToStringList(args[0]).andThen(xs =>
                    sexpToExp(args[1]).andThen(body =>
                      ok(elam(s.range, xs, body))))
              case 'if':
                return args.length !== 3
                  ? parserError(msg('error-arity', 'if', 3, args.length), s)
                  : sexpToExp(args[0]).andThen(e1 =>
                    sexpToExp(args[1]).andThen(e2 =>
                      sexpToExp(args[2]).andThen(e3 =>
                        ok(eif(s.range, e1, e2, e3)))))
              case 'let':
                return args.length !== 2
                  ? parserError(msg('error-arity', 'let', 2, args.length), s)
                  : sexpToBindings(args[0]).andThen(bindings =>
                    sexpToExp(args[1]).andThen(body =>
                      ok(elet(s.range, bindings, body))))
              case 'cond':
                return args.length === 0
                  ? parserError(msg('error-arity-atleast', 'cond', '1', args.length), s)
                  : join(args.map(sexpToBranch)).andThen(branches =>
                    ok(econd(s.range, branches)))
              case 'and':
                return join(args.map(sexpToExp)).andThen(es => ok(eand(s.range, es)))
              case 'or':
                return join(args.map(sexpToExp)).andThen(es => ok(eor(s.range, es)))
              default:
                // NOTE: applications whose head are not keywords are assumed to be applications.
                return join(args.map(sexpToExp)).andThen(es => ok(ecall(s.range, evar(head.range, head.single), es)))
            }
          case 'slist':
            return sexpToExp(head).andThen(e =>
              join(args.map(sexpToExp)).andThen(es =>
                ok(ecall(s.range, e, es))))
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
      } else {
        return sexpToExp(s).andThen(e => ok(sexp(e)))
      }
  }
}

function isImport (s: Sexp): boolean {
  return s.tag === 'slist' &&
    s.list.length === 2 &&
    (s.list[0].tag === 'atom' && (s.list[0] as Atom).single === 'import') &&
    s.list[1].tag === 'atom'
}

function findIndexOfFirstNonImport (ss: Sexp[]): number {
  for (let i = 0; i < ss.length; i++) {
    if (!isImport(ss[i])) { return i }
  }
  return -1
}

function sexpToImport_ (s: Sexp): string {
  return ((s as SList).list[1] as Atom).single
}

function sexpsToProgram (ss: Sexp[]): Result<Program> {
  const i = findIndexOfFirstNonImport(ss)
  const importsSexps = ss.slice(0, i)
  const stmtsSexps = ss.slice(i)
  return join(stmtsSexps.map(sexpToStmt)).andThen(statements => ok({
    imports: importsSexps.map(sexpToImport_),
    statements
  }))
}

function parseProgram (src: string): Result<Program> {
  return stringToSexps(src).andThen(ss => sexpsToProgram(ss))
}

export {
  parseExp, parseProgram
}
