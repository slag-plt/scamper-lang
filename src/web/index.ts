import * as Image from './image.js'
import * as Scamper from '../index.js'

// injects 
const global = window as any

global.replaceCodeWidgets = function (): void {
  for (const element of document.getElementsByClassName('scamper')) {
    const src = element.textContent!
    const result = Scamper.compileProgram(src).andThen(prog =>
      Scamper.ok({
        statements: prog.statements,
        outputs: new Scamper.ProgramState(prog).evaluate().prog.statements
      }))
    if (result.tag === 'error') {
      element.innerHTML = Scamper.errorToString(result)
    } else {
      element.innerHTML = ""
      for (var i = 0; i < result.value.statements.length; i++) {
        element.innerHTML += [
          `&gt; ${Scamper.stmtToString(0, result.value.statements[i], false, true)}`,
          Scamper.stmtToString(0, result.value.outputs[i], true, true),
          // N.B., extra spacing to 
          '',   
          ''
        ].join('\n')
      }

      // Now, sweep over the document and render any rich values that the runtime produced.
      Array.from(document.getElementsByClassName('drawing')).forEach(Image.emitDrawingWidget)
    }
  }
}