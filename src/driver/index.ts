import * as fs from 'fs'
import * as scamper from '../index.js'

type CompilerOptions = {
  filename?: string,
  checkOnly: boolean,
  formatOnly: boolean,
  emitTokens: boolean,
  emitTrace: boolean,
}

function makeDefaultOptions(): CompilerOptions {
  return {
    filename: undefined,
    checkOnly: false,
    formatOnly: false,
    emitTokens: false,
    emitTrace: false,
  }
}

function printHelp(): void {
  console.log(`
The Scamper command-line driver.

Usage: scamper [options] [filename] 

Options:
-h, --help       Prints this help message.
-c, --check      Checks the program for errors, but does not run the program.
--format         Formats the program, printing the results to stdout.
--emit-tokens    Prints tokens emitted by the lexer.
--emit-trace     Prints the step-by-step evaluation of the program.
  `.trim())
}

function processArgs(args: string[]): CompilerOptions {
  const opts = makeDefaultOptions()
  while (args.length > 0) {
    const arg = args.shift()!
    if (arg === '-h' || arg === '--help') {
      printHelp()
      process.exit(0)
    } else if (arg === '--check' || arg === '-c') {
      opts.checkOnly = true
    } else if (arg === '--format') {
      opts.formatOnly = true
    } else if (arg === '--emit-tokens') {
      opts.emitTokens = true
    } else if (arg === '--emit-trace') {
      opts.emitTrace = true
    } else if (arg.startsWith('-') || arg.startsWith('--')) {
      console.log(`Unknown option: ${arg}`)
      printHelp()
      process.exit(1)
    } else if (opts.filename !== undefined) {
      console.log(`Only one filename can be specified: ${arg}`)
      opts.filename = arg
    } else {
      opts.filename = arg
    }
  }   
  return opts
}

function emitTokens(tokens: scamper.sexp.Token[]) {
  tokens.forEach(tok => {
    // N.B., we 0-index line/col numbers to align with vscode, but for testing
    // purposes, it is much easier to work with 1-indexed numbers that align
    // with what editors actually report.
    console.log(`
${tok.value}: (${tok.range.start.line + 1}, ${tok.range.start.column + 1}) => (${tok.range.end.line + 1}, ${tok.range.end.column + 1})
    `.trim())
  })
}

async function main() {
  const opts = processArgs(process.argv.slice(2))
  if (opts.filename === undefined) {
    console.log('No filename specified.')
    printHelp()
    process.exit(1)
  }

  fs.readFile(opts.filename, 'utf8', (error, src) => {
    if (error) { throw error }

    // Phase 1: Lexing
    const tokensResult = scamper.sexp.tokenize(src)
    if (tokensResult.tag === 'error') {
      console.log(scamper.allDetailsToCompleteString(tokensResult.details))
      process.exit(1)
    }
    const tokens = tokensResult.value

    if (opts.emitTokens) { emitTokens(tokens) }

    // Phase 2: Parsing
    const sexpResult = scamper.sexp.tokensToSexps(tokens)
    if (sexpResult.tag === 'error') {
      console.log(scamper.allDetailsToCompleteString(sexpResult.details))
      process.exit(1)
    }
    const sexps = sexpResult.value

    const parseResult = scamper.parser.sexpsToProgram(sexps)
    if (parseResult.tag === 'error') {
      console.log(scamper.allDetailsToCompleteString(parseResult.details))
      process.exit(1)
    }
    const prog = parseResult.value

    if (opts.formatOnly) {
      console.log(scamper.Formatter.format(src))
      process.exit(0)
    }
    
    // Phase 2: Static checking
    const checkResult = scamper.detailsToResult(
      scamper.scope.scopeCheckProgram(prog))
    if (checkResult.tag === 'error') {
      console.log(scamper.allDetailsToCompleteString(checkResult.details))
      process.exit(1)
    }

    if (opts.checkOnly) { process.exit(0) }

    // Phase 3: Output
    if (opts.emitTrace) {
      let state = new scamper.ProgramState(prog)
      let step = 1
      console.log('===== Initial Program =====')
      console.log(state.toString())
      while (!state.isFullyEvaluated()) {
        state = state.step()
        console.log(`===== Step ${step++} =====`)
        console.log(state.toString())
      }
    } else {
      console.log(new scamper.ProgramState(prog).evaluate().toString())
    }
    process.exit(0)
  })
}

main()