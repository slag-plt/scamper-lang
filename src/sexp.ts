import { Result, ok, error, rethrow } from './result.js'
import { Loc, mkLoc, noLoc, Range, mkRange, noRange } from './loc.js'
import { msg } from './messages.js'

/* eslint-disable no-use-before-define */
type Sexp = Atom | SList

/** A single atom */
type Atom = { tag: 'atom'; single: string; range: Range; toString: () => string }
function atom (range: Range, single: string): Atom {
  return {
    tag: 'atom',
    single,
    range,
    toString: () => single
  }
}

/** An list of sexps */
type SList = { tag: 'slist'; list: Sexp[]; range: Range; toString: () => string }
function slist (range: Range, list: Sexp[]): SList {
  return {
    tag: 'slist',
    list,
    range,
    toString: () => `(${list.map(s => s.toString()).join(' ')})`
  }
}

type Token = { value: string, range: Range }
const mkToken = (value: string, loc: Loc): Token => ({ value, range: mkRange(loc, mkLoc(loc.line, loc.column + value.length - 1)) })

function lexerError <T> (message: string, tok?: Token, hint?: string): Result<T> {
  return error(msg('phase-lexer'), message, tok?.range, tok?.value, hint)
}

type LexerState = { pos: Loc, i: number }

// Create tokens from a string of expressions
function tokenize (src: string): Token[] {
  const result: Token[] = []
  // `tok` is a mutable structure that keeps track of the current token we are
  // building up, if we are building up one currently. When we are not building
  // up a token, `text` is undefined.
  const tok: { text?: string, start: Loc } = { text: undefined, start: noLoc() }
  // N.B., vscode 0-indexes line/col information!?

  const st: LexerState = { pos: mkLoc(0, 0), i: 0 }
  while (st.i < src.length) {
    const isWhitespace = /\s/.test(src[st.i])

    // 1. Check if character is either bracket or comma
    if (src[st.i] === '(' || src[st.i] === ')' || src[st.i] === ',') {
      // If we were previous tracking a token, then the bracket or comma marks its end.
      if (tok.text !== undefined) {
        result.push(mkToken(tok.text, mkLoc(tok.start.line, tok.start.column)))
        tok.text = undefined
        tok.start = noLoc()
      }
      result.push(mkToken(src[st.i], mkLoc(st.pos.line, st.pos.column)))
      st.i++
    // 2. Check if we hit a whitespace. Whitespace terminates the current token
    // we're tracking, if we are tracking one right now.
    } else if (isWhitespace) {
      if (tok.text !== undefined) {
        result.push(mkToken(tok.text, mkLoc(tok.start.line, tok.start.column)))
        tok.text = undefined
        tok.start = noLoc()
      }
      st.i++
    // 3. Any other characters are tracked as a multi-character token.
    } else {
      // N.B., set the start position for the token if it has not yet been initialized.
      if (tok.text === undefined) {
        tok.text = src[st.i]
        tok.start = mkLoc(st.pos.line, st.pos.column)
      } else {
        tok.text += src[st.i]
      }
      st.i++
    }
    // 4. Update our current position based on the token we read.
    // TODO: do we need to chomp \r\n as a newline for Windows-style files?
    if (src[st.i] === '\n' || src[st.i] === '\r') {
      st.pos.line += 1
      st.pos.column = 0
    } else {
      st.pos.column += 1
    }
  }
  // After processing the text, push the last token if we are tracking one
  if (tok.text !== undefined) {
    result.push(mkToken(tok.text, mkLoc(tok.start.line, tok.start.column)))
  }
  return result
}

//
function tokensToSListArgs (toks: Token[]): Result<Sexp[]> {
  if (toks.length === 0) {
    return lexerError(msg('error-eof'))
  }
  const sexps: Sexp[] = []
  while (toks.length > 0 && toks[0].value !== ')') {
    const next = tokensToSexp(toks)
    switch (next.tag) {
      case 'error': return rethrow(next)
      case 'ok': {
        sexps.push(next.value)
      }
    }
  }
  if (toks.length === 0) {
    return error(msg('phase-lexer'), msg('error-missing-parens'),
      sexps.length > 0
        ? mkRange(sexps[0].range.start, sexps[sexps.length - 1].range.end)
        : noRange())
  }
  toks.shift() // N.B., pop the ')'
  return ok(sexps)
}

function tokensToSexp (toks: Token[]): Result<Sexp> {
  if (toks.length === 0) {
    return lexerError(msg('error-eof'))
  } else {
    const head = toks.shift()!
    switch (head.value) {
      case '(': {
        return tokensToSListArgs(toks).andThen((args) => ok(slist(mkRange(args[0].range.start, args[args.length - 1].range.end), args)))
      }
      case ',': {
        return tokensToSexp(toks)
      }
      case ')': return lexerError(msg('error-unmatched-parens'), head)
      default: return ok(atom(head.range, head.value))
    }
  }
}

function stringToSexp (s: string): Result<Sexp> {
  return tokensToSexp(tokenize(s))
}

function stringToSexps (s: string): Result<Sexp[]> {
  const toks = tokenize(s)
  const result = []
  while (toks.length > 0) {
    const next = tokensToSexp(toks)
    switch (next.tag) {
      case 'error': return rethrow(next)
      case 'ok': result.push(next.value)
    }
  }
  return ok(result)
}

function sexpToString (s: Sexp): string {
  if (s.tag === 'atom') {
    return s.single
  } else {
    return `(${s.list.map(sexpToString).join(' ')})`
  }
}

export {
  Sexp, Atom, SList, atom, slist,
  tokenize, stringToSexp, stringToSexps,
  sexpToString
}
