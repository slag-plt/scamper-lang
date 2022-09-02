import * as Image from './image.js'
import * as Music from './music.js'
import * as Scamper from '../index.js'
import * as JZZ from './jzz/jzz-combined.cjs'

// N.B., this module injects functions directly into window so that pages can
// call the setup functions as needed.
const global = window as any

// The singleton instance of the JZZ.Tiny synth
const SYNTH = JZZ.synth.Tiny()

function forEachByClass(elt: Element | Document, cls: string, fn: (e: Element) => void) {
  // N.B., we need to freeze the list of elements to an array because fn
  // may manipulate the DOM and getElementsByClassName returns a live
  // collection. The result is that DOM manipulation messes up indexing
  // during the loop.
  Array.from(elt.getElementsByClassName(cls)).forEach(e => fn(e))
}

function renderRichWidgets(root: Element | Document): void {
  forEachByClass(document, 'drawing', Image.emitDrawingWidget)
  forEachByClass(document, 'composition', e => Music.emitCompositionWidget(SYNTH, e))
}

function replaceOutputWidgets() {
  for (const element of document.getElementsByClassName('scamper-output')) {
    const classes = element.className.split(' ')
    const outputProg = classes.includes('output-prog')
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
        if (outputProg) {
          element.innerHTML += [
            `&gt; ${Scamper.stmtToString(0, result.value.statements[i], false, true)}`,
            Scamper.stmtToString(0, result.value.outputs[i], true, true),
            // N.B., extra spacing to 
            '',   
            ''
          ].join('\n')
        } else {
          const line = Scamper.stmtToString(0, result.value.outputs[i], false, true)
          if (line.trim().length > 0) {
            element.innerHTML += `${line}\n`
          }
        }
      }
      renderRichWidgets(element)
    }
  }
}

function replaceExplorationWidgets(): void {
  for (const element of document.getElementsByClassName('scamper-exploration')) {
    // Look for the program element in the exploration window and signal a hard
    // error if we can't find it. We need the program element as it should
    // contain the code we need to run!
    const progElements = element.getElementsByClassName('program')
    if (progElements.length !== 1) {
      element.innerHTML = 'Error: there must be exactly one program element in this widget'
      return
    }
    const programElement = progElements[0]

    const src = programElement.textContent!
    const result = Scamper.compileProgram(src)
    if (result.tag === 'error') {
      element.innerHTML = Scamper.errorToString(result)
      return
    } else {
      const trace = new Scamper.ProgramTrace(new Scamper.ProgramState(result.value))
      const update = () => {
        forEachByClass(element, 'step-counter', e => { e.innerHTML = `Step ${trace.currentStep()}` })
        programElement.innerHTML = Scamper.progToString(0, trace.currentState().prog, true, true)
        renderRichWidgets(programElement)
      }
    
      // Rig the buttons that are present in the widget
      forEachByClass(element, 'step-forward', e => {
        (e as HTMLButtonElement).onclick = () => { trace.stepForward(); update(); }
      })
      forEachByClass(element, 'step-backward', e => {
        (e as HTMLButtonElement).onclick = () => { trace.stepBackward(); update(); }
      })
      forEachByClass(element, 'stmt-forward', e => {
        (e as HTMLButtonElement).onclick = () => { trace.evalNextStmt(); update(); }
      })
      forEachByClass(element, 'stmt-backward', e => {
        (e as HTMLButtonElement).onclick = () => { trace.revertPrevStmt(); update(); }
      })
      forEachByClass(element, 'eval', e => {
        (e as HTMLButtonElement).onclick = () => { trace.evaluateProg(); update(); }
      })
      forEachByClass(element, 'reset', e => {
        (e as HTMLButtonElement).onclick = () => { trace.resetProg(); update(); }
      })
      forEachByClass(element, 'add-statement', e => {
        (e as HTMLButtonElement).onclick = () => {
          // N.B., should really be an id, but the need to give the input a globally
          // unique id!
          const inputs = document.getElementsByClassName('add-statement-input')
          if (inputs.length > 0) {
            const input = inputs[0] as HTMLInputElement
            if (input.value.length > 0) {
              trace.addStmt(input.value)
              trace.evaluateProg()
              input.value = ''
            }
          }
        }
      })
    } 
  }
}

global.replaceCodeWidgets = function (): void {
  replaceOutputWidgets()
  replaceExplorationWidgets()
}