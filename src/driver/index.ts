#!/usr/bin/env node

import * as fs from 'fs'
import * as scamper from '../index.js'

class NodeVFS implements scamper.Vfs.VFSProvider {
  read (path: string): Promise<scamper.Result<string>> {
    try {
      return Promise.resolve(scamper.ok(fs.readFileSync(path).toString()))
    } catch (e) {
      return Promise.resolve(scamper.Vfs.fileNotFoundError(path))
    }
  }

  write (path: string, content: string): Promise<scamper.Result<void>> {
    throw new Error('Method not implemented.')
  }
}

type CompilerOptions = {
  filename?: string,
  checkOnly: boolean,
  formatOnly: boolean,
  emitProg: boolean,
  emitTokens: boolean,
  emitTrace: boolean,
  useStepper: boolean,
}

function makeDefaultOptions (): CompilerOptions {
  return {
    filename: undefined,
    checkOnly: false,
    formatOnly: false,
    emitProg: false,
    emitTokens: false,
    emitTrace: false,
    useStepper: false
  }
}

function printHelp (): void {
  console.log(`
The Scamper command-line driver.

Usage: scamper [options] [filename] 

Options:
-h, --help       Prints this help message.
-c, --check      Checks the program for errors, but does not run the program.
--format         Formats the program, printing the results to stdout.
--emit-prog      Prints the output interspersed between the program's statements.
--emit-tokens    Prints tokens emitted by the lexer.
--emit-trace     Prints the step-by-step evaluation of the program (implies --use-stepper).
--use-stepper    Uses the stepper instead of the evaluator.
  `.trim())
}

function processArgs (args: string[]): CompilerOptions {
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
    } else if (arg === '--emit-prog') {
      opts.emitProg = true
    } else if (arg === '--emit-tokens') {
      opts.emitTokens = true
    } else if (arg === '--emit-trace') {
      opts.emitTrace = true
    } else if (arg === '--use-stepper') {
      opts.useStepper = true
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

function emitTokens (tokens: scamper.sexp.Token[]) {
  tokens.forEach(tok => {
    // N.B., we 0-index line/col numbers to align with vscode, but for testing
    // purposes, it is much easier to work with 1-indexed numbers that align
    // with what editors actually report.
    console.log(`
${tok.value}: (${tok.range.start.line + 1}, ${tok.range.start.column + 1}) => (${tok.range.end.line + 1}, ${tok.range.end.column + 1})
    `.trim())
  })
}

function main () {
  const opts = processArgs(process.argv.slice(2))
  if (opts.filename === undefined) {
    console.log('No filename specified.')
    printHelp()
    process.exit(1)
  }

  scamper.Vfs.fs.mount('', new NodeVFS())

  // eslint-disable-next-line @typescript-eslint/no-misused-promises
  fs.readFile(opts.filename, 'utf8', async (error, src) => {
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
        state = await state.step()
        console.log(`===== Step ${step++} =====`)
        console.log(state.toString())
      }
    } else if (opts.useStepper) {
      console.log((await new scamper.ProgramState(prog).evaluate()).toString())
    } else if (opts.emitProg) {
      const effects = await scamper.evaluateProgram(prog, false)
      const endLocs = prog.map(s => s.range.end)
      const segments = scamper.splitByLocs(endLocs, src)
      const lines: string[] = []
      prog.forEach((s, i) => {
        const segment = segments[i]
        const output = scamper.effectToString(0, effects[i], true)
        if (output.length > 0) {
          lines.push(`${segment}\n`)
          lines.push(`> ${output}`)
        } else {
          lines.push(segment)
        }
      })
      lines.push(segments[segments.length - 1])
      console.log(lines.join(''))
    } else {
      const effects = await scamper.evaluateProgram(prog, false)
      console.log(effects.map(fx => scamper.effectToString(0, fx, false)).filter(s => s.length > 0).join('\n'))
    }
    process.exit(0)
  })
}

main()
