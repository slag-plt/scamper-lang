import { ProgramState, compileProgram, progToString, stmtToString, Formatter } from './index.js'
import { Program, isOutputEffect } from './lang.js'
import { errorToString } from './result.js'
import { tokenize } from './sexp.js'

import * as fs from 'fs'

const mode = process.argv[2]
const filename = process.argv[3]

function lex (src: string): void {
  const result = tokenize(src)
  if (result.tag === 'error') {
    console.log(errorToString(result))
  } else {
    result.value.forEach(tok => {
      // N.B., we 0-index line/col numbers to align with vscode, but for testing
      // purposes, it is much easier to work with 1-indexed numbers that align
      // with what editors actually report.
      console.log(`${tok.value}: (${tok.range.start.line + 1}, ${tok.range.start.column + 1}) => (${tok.range.end.line + 1}, ${tok.range.end.column + 1})`)
    })
  }
}

function formatProgram (src: string): void {
  return console.log(Formatter.format(src))
}

function runProgram (prog: Program) {
  const st = new ProgramState(prog).evaluate()
  st.prog.statements.forEach(s => {
    const text = stmtToString(0, s)
    if (text !== '') {
      console.log(text)
    }
  })
}

function traceProgram (prog: Program) {
  let st = new ProgramState(prog)
  console.log('===== Program =====')
  console.log(progToString(0, st.prog))
  console.log()
  console.log('===== Evaluation =====')
  let count = 1
  while (!st.isFullyEvaluated()) {
    st = st.step()
    console.log(`Step ${count++}:`)
    console.log(progToString(0, st.prog, true))
  }
  console.log('===== Output =====')
  st.prog.statements.forEach(s => {
    if (isOutputEffect(s)) {
      console.log(`${stmtToString(0, s)}`)
    }
  })
}

fs.readFile(filename, 'utf8', (error, src) => {
  if (error) { throw error }
  if (mode === 'lex') {
    lex(src)
  } else {
    const result = compileProgram(src)
    switch (result.tag) {
      case 'error':
        console.log(errorToString(result)) // TODO: pretty print this error!
        break
      case 'ok': {
        switch (mode) {
          case 'output':
            runProgram(result.value)
            break
          case 'trace':
            traceProgram(result.value)
            break
          case 'format':
            formatProgram(src)
            break
          default:
            console.log(`Unrecognized driver command: ${mode}`)
            break
        }
      }
    }
  }
})
