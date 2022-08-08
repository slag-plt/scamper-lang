import { Result, ok, error, rethrow, ICE } from './result.js'
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

/**
 * The state of the lexer updated throughout the lexing process:
 * + `i` is the current position of the lexer in the source string.
 * +`pos` is the current position of the lexer in the source file.
 * + `tok` is a mutable structure that keeps track of the current token we are
 *    building up, if we are building up one currently. When we are not
 *    building up a token, `text` is undefined.
 */
type TokenState = { text?: string, start: Loc }
type LexerState = { i: number, pos: Loc, tok: TokenState }

function resetTokenState (st: LexerState): void {
  st.tok.text = undefined
  st.tok.start = noLoc()
}

function startTracking (st: LexerState, ch: string): void {
  st.tok.text = ch
  st.tok.start = mkLoc(st.pos.line, st.pos.column)
}

function emitToken (st: LexerState): Token {
  const result = {
    value: st.tok.text!,
    // N.B., assumes that the lexer state ends one position past the token
    // currently being built.
    range: mkRange(
      mkLoc(st.tok.start.line, st.tok.start.column),
      mkLoc(st.pos.line, st.pos.column))
  }
  resetTokenState(st)
  return result
}

function tokenizeStringLiteral (src: string, st: LexerState): Result<Token> {
  if (src[st.i] !== '"') {
    throw new ICE('tokenizeStringLiteral', `Beginning character is not a quote: ${src[st.i]}`)
  } else {
    // Advance past the open quote, starting the token we're tracking
    startTracking(st, '"')
    st.i += 1
    while (st.i < src.length) {
      // A quote closes this string literal
      if (src[st.i] === '"') {
        st.tok.text += '"'
        st.pos.column += 1
        st.i += 1
        return ok(emitToken(st))
      // Escape characters require us to consume the next character
      } else if (src[st.i] === '\\') {
        if (st.i + 1 === src.length) {
          return lexerError(msg('error-eof-string'))
        } else {
          const ch = src[st.i + 1]
          if (ch === 'a') {
            // Alarm: ASCII 7
            st.tok.text += '\u0007'
            st.pos.column += 2
          } else if (ch === 'b') {
            // Backspace: ASCII 8
            st.tok.text += '\u0008'
            st.pos.column += 2
          } else if (ch === 't') {
            // Tab: ASCII 9
            st.tok.text += '\u0009'
            st.pos.column += 2
          } else if (ch === 'n') {
            // Linefeed: ASCII 10
            st.tok.text += '\u000A'
            st.pos.column += 2
          } else if (ch === 'v') {
            // Vertical tab: ASCII 11
            st.tok.text += '\u000B'
            st.pos.column += 2
          } else if (ch === 'f') {
            // Form feed: ASCII 12
            st.tok.text += '\u000C'
            st.pos.column += 2
          } else if (ch === 'r') {
            // Carriage return: ASCII 13
            st.tok.text += '\u000D'
            st.pos.column += 2
          } else if (ch === 'e') {
            // Escape: ASCII 27
            st.tok.text += '\u001B'
            st.pos.column += 2
          } else if (ch === '"') {
            st.tok.text += '"'
            st.pos.column += 2
          } else if (ch === '\'') {
            st.tok.text += '\''
            st.pos.column += 2
          } else if (ch === '\\') {
            st.tok.text += '\\'
            st.pos.column += 2
          } else if (ch >= '0' && ch <= '9') {
            throw new ICE('tokenizeStringLiteral', 'Octal escape not supported')
          } else if (ch === 'x') {
            throw new ICE('tokenizeStringLiteral', 'Hex escape not supported')
          } else if (ch === 'u' || ch === 'U') {
            throw new ICE('tokenizeStringLiteral', 'Unicode escape not supported')
          } else if (ch === '\n') {
            // Skip over newline characters but continue processing the literal
            st.pos.line += 1
            st.pos.column = 0
          } else {
            return lexerError(msg('error-unrecognized-escape', ch))
          }
          st.i += 2 // N.B., we moved forward two characters in the source at this point!
        }
      } else {
        st.tok.text += src[st.i++]
      }
    }
    // Eek, if we get this far, then we have exhausted the input without seeing a close quote!
    return lexerError(msg('error-eof-string'))
  }
}

// Create tokens from a string of expressions
function tokenize (src: string): Result<Token[]> {
  const result: Token[] = []
  // N.B., vscode 0-indexes line/col information!?
  const st: LexerState = { i: 0, pos: mkLoc(0, 0), tok: { text: undefined, start: noLoc() } }
  while (st.i < src.length) {
    const isWhitespace = /\s/.test(src[st.i])

    // 1. Check if character is either bracket or comma
    if (src[st.i] === '(' || src[st.i] === ')' || src[st.i] === ',') {
      // If we were previous tracking a token, then the bracket or comma marks its end.
      if (st.tok.text !== undefined) {
        result.push(emitToken(st))
      }
      startTracking(st, src[st.i])
      result.push(emitToken(st))
      st.i++
    // 2. Check if we hit a whitespace. Whitespace terminates the current token
    // we're tracking, if we are tracking one right now.
    } else if (isWhitespace) {
      if (st.tok.text !== undefined) {
        result.push(emitToken(st))
      }
      st.i++
    // 3. Check if we hit a quote. If so, then parse a quote.
    } else if (src[st.i] === '"') {
      if (st.tok.text !== undefined) {
        result.push(emitToken(st))
      }
      const lit = tokenizeStringLiteral(src, st)
      switch (lit.tag) {
        case 'error':
          return rethrow(lit)
        case 'ok':
          result.push(lit.value)
      }
    // 4. Any other characters are tracked as a multi-character token.
    } else {
      // N.B., set the start position for the token if it has not yet been initialized.
      if (st.tok.text === undefined) {
        startTracking(st, src[st.i])
      } else {
        st.tok.text += src[st.i]
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
  if (st.tok.text !== undefined) {
    result.push(mkToken(st.tok.text, mkLoc(st.tok.start.line, st.tok.start.column)))
  }
  return ok(result)
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
  return tokenize(s).andThen(toks => tokensToSexp(toks))
}

function stringToSexps (s: string): Result<Sexp[]> {
  return tokenize(s).andThen(toks => {
    const result = []
    while (toks.length > 0) {
      const next = tokensToSexp(toks)
      switch (next.tag) {
        case 'error': return rethrow(next)
        case 'ok': result.push(next.value)
      }
    }
    return ok(result)
  })
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
