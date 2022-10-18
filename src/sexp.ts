import { Result, ok, error, rethrow, ICE } from './result.js'
import { Loc, mkLoc, Range, mkRange, noRange } from './loc.js'
import { msg } from './messages.js'

/* eslint-disable no-use-before-define */
type BracketKind = '(' | '[' | '{'

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
type SList = { tag: 'slist'; list: Sexp[]; range: Range; bracket: BracketKind, toString: () => string }
function slist (range: Range, bracket: BracketKind, list: Sexp[]): SList {
  return {
    tag: 'slist',
    list,
    range,
    bracket,
    toString: () => `(${list.map(s => s.toString()).join(' ')})`
  }
}

type Token = { value: string, range: Range }
const mkStartingToken = (value: string, start: Loc) => ({
  value,
  // N.B., Clone locs to allow us to safely mutate them during tokenization
  range: mkRange(mkLoc(start.line, start.column), mkLoc(start.line, start.column))
})

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
class LexerState {
  i: number
  pos: Loc
  tok: Token | undefined

  constructor () {
    this.i = 0
    this.pos = mkLoc(0, 0)
    this.tok = undefined
  }

  public isTracking (): boolean {
    return this.tok !== undefined
  }

  public startTracking (ch: string): void {
    if (this.tok !== undefined) {
      throw new ICE('startTracking', 'startTracking called when a token is already being built')
    } else {
      this.tok = mkStartingToken(ch, this.pos)
    }
  }

  public resetTokenState (): void {
    this.tok = undefined
  }

  public emitToken (): Token {
    if (this.tok === undefined) {
      throw new ICE('emitToken', 'emitToken called when a token is not yet being built')
    } else {
      const result = this.tok
      this.resetTokenState()
      return result
    }
  }

  public advanceColumn (): void {
    this.i += 1
    this.pos.column += 1
  }

  public advanceLine (): void {
    this.i += 1
    this.pos.line += 1
    this.pos.column = 0
  }

  public append (s: string) {
    if (this.tok === undefined) {
      throw new ICE('append', 'append called when a token is not yet being built')
    } else {
      this.tok.value += s
      this.tok.range.end.line = this.pos.line
      this.tok.range.end.column = this.pos.column
    }
  }
}

function tokenizeWord (word: string, src: string, st: LexerState): Result<null> {
  for (let i = 0; i < word.length; i++) {
    if (word[i] !== src[st.i]) {
      throw new ICE('tokenizeWord', `Encountered ${src[st.i]} while parsing ${word}`)
    } else {
      st.append(src[st.i])
      st.advanceColumn()
    }
  }
  return ok(null)
}

function tokenizeLineComment (src: string, st: LexerState): Token {
  if (src[st.i] !== ';') {
    throw new ICE('tokenizeLineComment', `Beginning character is not a semicolon: ${src[st.i]}`)
  } else {
    st.startTracking('"')
    st.advanceColumn()
    while (st.i < src.length) {
      // A newline character ends this comment
      if (src[st.i] === '\n') {
        st.advanceLine()
        return st.emitToken()

      // Any other character is sucked into the comment
      } else {
        st.append(src[st.i])
        st.advanceColumn()
      }
    }
  }
  // If we hit the end of the file, then this final token is a comment
  return st.emitToken()
}

function tokenizeBlockComment (src: string, st: LexerState): Result<Token> {
  if (st.i >= src.length - 1 || src[st.i] !== '#' || src[st.i + 1] !== '|') {
    throw new ICE('tokenizeBlockComment', `Beginning characters do not start a block comment: ${src[st.i]}${src[st.i + 1]}`)
  } else {
    let blockLevel = 0
    st.startTracking('#|')
    st.advanceColumn()
    st.advanceColumn()
    while (st.i < src.length - 1) {
      // A '|#' sequence ends the current block comment. Block comments can be
      // nested, so ensure that we stop only when we've closed the outermost
      // block.
      if (src[st.i] === '|' && src[st.i + 1] === '#') {
        blockLevel -= 1
        if (blockLevel < 0) {
          st.advanceColumn()
          st.advanceColumn()
          return ok(st.emitToken())
        }
      } else if (src[st.i] === '#' && src[st.i + 1] === '|') {
        blockLevel += 1
        st.advanceColumn()
        st.advanceColumn()
      } else if (src[st.i] === '\n') {
        st.append('\n')
        st.advanceLine()
      } else {
        st.append(src[st.i])
        st.advanceColumn()
      }
    }
    // We did not terminate the block comment; raise an error!
    return lexerError(msg('error-eof-block-comment'))
  }
}

function tokenizeCharLiteral (src: string, st: LexerState): Result<Token> {
  const namedChars = [
    'alarm',
    'backspace',
    'delete',
    'escape',
    'newline',
    'null',
    'return',
    'space',
    'tab'
  ]
  const startsWithNamedChar = (src: string, st: LexerState): string | undefined => {
    for (const n of namedChars) {
      if (src.startsWith(n, st.i)) {
        return n
      }
    }
    return undefined
  }
  // N.B., the Racket standard defines leading "terminators" for char constants
  // to be "(non-)alphabetic" characters. Just maximally chomping the token
  // seems much more sensible.
  const isWhitespace = (c: string): boolean => /\s/.test(c)
  const isBracket = (c: string): boolean =>
    c === '(' || c === ')' || c === '[' || c === ']' || c === '{' || c === '}'

  if (!(st.i < src.length - 1) && src[st.i] !== '#' && src[st.i + 1] !== '\\') {
    throw new ICE('tokenizeCharLiteral', `Beginning characters are not hash-slash: ${src[st.i]}`)
  } else {
    st.startTracking('#\\')
    st.advanceColumn()
    st.advanceColumn()
    const namedChar = startsWithNamedChar(src, st)
    if (namedChar !== undefined) {
      tokenizeWord(namedChar, src, st)
      if (st.i >= src.length || (isWhitespace(src[st.i]) || isBracket(src[st.i]))) {
        return ok(st.emitToken())
      } else {
        // N.B., add the offending character onto the end of the "bad" token
        // and spit out an error.
        st.append(src[st.i])
        if (src[st.i] === '\n') {
          st.advanceLine()
        } else {
          st.advanceColumn()
        }
        const tok = st.emitToken()
        return lexerError(msg('error-invalid-char-constant'), tok)
      }
    } else if (st.i >= src.length - 1 ||
               (isWhitespace(src[st.i]) && !isWhitespace(src[st.i + 1])) ||
               (!isWhitespace(src[st.i]) && (isWhitespace(src[st.i + 1]) ||
                                             isBracket(src[st.i + 1])))) {
      st.append(src[st.i])
      if (src[st.i] === '\n') {
        st.advanceLine()
      } else {
        st.advanceColumn()
      }
      return ok(st.emitToken())
    } else {
      // TODO: ugh, should update line/col info here, but it is a pain.
      // Really need to refactor the code so that line/col updates are
      // handled by LexerState.
      st.append(src[st.i])
      st.append(src[st.i + 1])
      const tok = st.emitToken()
      return lexerError(msg('error-invalid-char-constant'), tok)
    }
  }
}

function tokenizeStringLiteral (src: string, st: LexerState): Result<Token> {
  if (src[st.i] !== '"') {
    throw new ICE('tokenizeStringLiteral', `Beginning character is not a quote: ${src[st.i]}`)
  } else {
    // Advance past the open quote, starting the token we're tracking
    st.startTracking('"')
    st.advanceColumn()
    while (st.i < src.length) {
      // A quote closes this string literal
      if (src[st.i] === '"') {
        st.append('"')
        st.advanceColumn()
        return ok(st.emitToken())
      // Escape characters require us to consume the next character
      } else if (src[st.i] === '\\') {
        if (st.i + 1 === src.length) {
          return lexerError(msg('error-eof-string'))
        } else {
          const ch = src[st.i + 1]
          if (ch === 'a') {
            // Alarm: ASCII 7
            st.append('\u0007')
            st.advanceColumn()
            st.advanceColumn()
          } else if (ch === 'b') {
            // Backspace: ASCII 8
            st.append('\u0008')
            st.advanceColumn()
            st.advanceColumn()
          } else if (ch === 't') {
            // Tab: ASCII 9
            st.append('\u0009')
            st.advanceColumn()
            st.advanceColumn()
          } else if (ch === 'n') {
            // Linefeed: ASCII 10
            st.append('\u000A')
            st.advanceColumn()
            st.advanceColumn()
          } else if (ch === 'v') {
            // Vertical tab: ASCII 11
            st.append('\u000B')
            st.advanceColumn()
            st.advanceColumn()
          } else if (ch === 'f') {
            // Form feed: ASCII 12
            st.append('\u000C')
            st.advanceColumn()
            st.advanceColumn()
          } else if (ch === 'r') {
            // Carriage return: ASCII 13
            st.append('\u000D')
            st.advanceColumn()
            st.advanceColumn()
          } else if (ch === 'e') {
            // Escape: ASCII 27
            st.append('\u001B')
            st.advanceColumn()
            st.advanceColumn()
          } else if (ch === '"') {
            st.append('"')
            st.advanceColumn()
            st.advanceColumn()
          } else if (ch === '\'') {
            st.append('\'')
            st.advanceColumn()
            st.advanceColumn()
          } else if (ch === '\\') {
            st.append('\\')
            st.advanceColumn()
            st.advanceColumn()
          } else if (ch >= '0' && ch <= '9') {
            throw new ICE('tokenizeStringLiteral', 'Octal escape not supported')
          } else if (ch === 'x') {
            throw new ICE('tokenizeStringLiteral', 'Hex escape not supported')
          } else if (ch === 'u' || ch === 'U') {
            throw new ICE('tokenizeStringLiteral', 'Unicode escape not supported')
          } else if (ch === '\n') {
            // Skip over newline characters but continue processing the literal
            st.advanceColumn()
            st.advanceLine()
          } else {
            return lexerError(msg('error-unrecognized-escape', ch))
          }
        }
      } else {
        st.append(src[st.i])
        st.advanceColumn()
      }
    }
    // Eek, if we get this far, then we have exhausted the input without seeing a close quote!
    return lexerError(msg('error-eof-string'))
  }
}

// Create tokens from a string of expressions
function tokenize (src: string): Result<Token[]> {
  const result: Token[] = []
  const st = new LexerState()
  while (st.i < src.length) {
    const isWhitespace = /\s/.test(src[st.i])

    // 1. Check if character is either bracket or comma
    if (src[st.i] === '(' || src[st.i] === ')' ||
        src[st.i] === '[' || src[st.i] === ']' ||
        src[st.i] === '{' || src[st.i] === '}' || src[st.i] === ',') {
      // If we were previous tracking a token, then the bracket or comma marks its end.
      if (st.isTracking()) {
        result.push(st.emitToken())
      }
      st.startTracking(src[st.i])
      result.push(st.emitToken())
      st.advanceColumn()
    // 2. Check if we hit a whitespace. Whitespace terminates the current token
    // we're tracking, if we are tracking one right now.
    } else if (isWhitespace) {
      if (st.isTracking()) {
        result.push(st.emitToken())
      }
      if (src[st.i] === '\n') {
        st.advanceLine()
      } else {
        st.advanceColumn()
      }
    // 3. Check if we hit a quote. If so, then parse a quote.
    } else if (src[st.i] === '"') {
      if (st.isTracking()) {
        result.push(st.emitToken())
      }
      const lit = tokenizeStringLiteral(src, st)
      switch (lit.tag) {
        case 'error':
          return rethrow(lit)
        case 'ok':
          result.push(lit.value)
      }
    // 4. Check if we hit a semicolon. If so, then parse a line comment
    } else if (src[st.i] === ';') {
      if (st.isTracking()) {
        result.push(st.emitToken())
      }
      // eslint-disable-next-line no-unused-vars
      const _comment = tokenizeLineComment(src, st)
      // N.B., ignore the comment for now!
      // result.push(comment)
    // 5. Check if we hit '#|'. If so, then parse a block comment
    } else if (st.i < src.length - 1 && src[st.i] === '#' && src[st.i + 1] === '|') {
      const _comment = tokenizeBlockComment(src, st)
      switch (_comment.tag) {
        case 'error':
          return rethrow(_comment)
        case 'ok':
          // N.B., ignore the comment for now!
          // result.push(comment)
      }
    // 6. CHeck if we hit '#\'. If so, then parse a character literal.
    } else if (st.i < src.length - 1 && src[st.i] === '#' && src[st.i + 1] === '\\') {
      const charConst = tokenizeCharLiteral(src, st)
      switch (charConst.tag) {
        case 'error':
          return rethrow(charConst)
        case 'ok':
          result.push(charConst.value)
      }
    // 6. Any other characters are tracked as a multi-character token.
    } else {
      // N.B., set the start position for the token if it has not yet been initialized.
      if (!st.isTracking()) {
        st.startTracking(src[st.i])
      } else {
        st.append(src[st.i])
      }
      st.advanceColumn()
    }
  }
  // After processing the text, push the last token if we are tracking one
  if (st.isTracking()) {
    result.push(st.emitToken())
  }
  return ok(result)
}

function tokensToSListArgs (endBracket: string, toks: Token[]): Result<Sexp[]> {
  if (toks.length === 0) {
    return lexerError(msg('error-eof'))
  }
  const sexps: Sexp[] = []
  while (toks.length > 0 && toks[0].value !== endBracket) {
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
      case '(':
        return tokensToSListArgs(')', toks).andThen((args) =>
          args.length === 0
            ? ok(slist(head.range, '(', args))
            : ok(slist(mkRange(args[0].range.start, args[args.length - 1].range.end), '(', args)))
      case '[':
        return tokensToSListArgs(']', toks).andThen((args) =>
          args.length === 0
            ? ok(slist(head.range, '[', args))
            : ok(slist(mkRange(args[0].range.start, args[args.length - 1].range.end), '[', args)))
      case '{':
        return tokensToSListArgs('}', toks).andThen((args) =>
          args.length === 0
            ? ok(slist(head.range, '{', args))
            : ok(slist(mkRange(args[0].range.start, args[args.length - 1].range.end), '{', args)))
      case ',':
        return tokensToSexp(toks)
      case ')':
        return lexerError(msg('error-unmatched-parens'), head)
      case ']':
        return lexerError(msg('error-unmatched-parens'), head)
      case '}':
        return lexerError(msg('error-unmatched-parens'), head)
      default:
        return ok(atom(head.range, head.value))
    }
  }
}

function stringToSexp (s: string): Result<Sexp> {
  return tokenize(s).andThen(toks => tokensToSexp(toks))
}

function tokensToSexps (tokens: Token[]): Result<Sexp[]> {
  const result = []
  while (tokens.length > 0) {
    const next = tokensToSexp(tokens)
    switch (next.tag) {
      case 'error': return rethrow(next)
      case 'ok': result.push(next.value)
    }
  }
  return ok(result)
}

function stringToSexps (s: string): Result<Sexp[]> {
  return tokenize(s).andThen(tokensToSexps)
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
  Token, tokenize, tokensToSexp, stringToSexp, tokensToSexps, stringToSexps,
  sexpToString
}
