import { ProgramState, compileProgram } from './index.js'
import { expToString, progToString, Program, stmtToString } from './lang.js'
import { errorToString } from './result.js'

import * as fs from 'fs'

const mode = process.argv[2]
const filename = process.argv[3]

function runProgram (prog: Program) {
  const st = new ProgramState(prog).evaluate()
  st.prog.forEach(s => {
    if (s.tag === 'value') {
      console.log(expToString(s.value))
    }
  })
}

function traceProgram (prog: Program) {
  let st = new ProgramState(prog)
  console.log('===== Program =====')
  console.log(progToString(st.prog))
  console.log()
  console.log('===== Evaluation =====')
  let count = 1
  while (!st.isFullyEvaluated()) {
    st = st.step()
    console.log(`Step ${count++}:`)
    st.env.forEach((v, k) => { console.log(`${k} = ${expToString(v)}`) })
    console.log(progToString(st.prog))
  }
  console.log('===== Output =====')
  st.prog.forEach(s => console.log(`${stmtToString(s)}`))
}

fs.readFile(filename, 'utf8', (error, src) => {
  if (error) { throw error }
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
        default:
          console.log(`Unrecognized driver command: ${mode}`)
          break
      }
    }
  }
})
