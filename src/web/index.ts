/* eslint-disable @typescript-eslint/no-unsafe-call */
/* eslint-disable @typescript-eslint/no-unsafe-assignment */
/* eslint-disable @typescript-eslint/no-unsafe-member-access */
import * as Scamper from '../index.js'
import * as Pretty from './pretty.js'

import 'prismjs'

// N.B., this module injects functions directly into window so that pages can
// call the setup functions as needed. We also call on modules that attach
// to window that we assume that the page loads.
const global = window as any

// The Prism instance
declare let Prism: any

async function forEachByClass (elt: Element | Document, cls: string, fn: (e: Element) => Promise<void>) {
  // N.B., we need to freeze the list of elements to an array because fn
  // may manipulate the DOM and getElementsByClassName returns a live
  // collection. The result is that DOM manipulation messes up indexing
  // during the loop.
  const elts = Array.from(elt.getElementsByClassName(cls))
  for (const e of elts) {
    await fn(e)
  }
}

// N.B., maybe we want this to be more efficient...!
export const sanitize = (s: string): string =>
  s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')

function makeTag (tag: string, ...attrs: [string, string][]): HTMLElement {
  const ret = document.createElement(tag)
  for (const [k, v] of attrs) {
    ret.setAttribute(k, v)
  }
  return ret
}

async function replaceOutputWidgets () {
  for (const element of document.getElementsByClassName('scamper-output')) {
    const classes = element.className.split(' ')
    const outputProg = classes.includes('output-prog')
    const src = element.textContent!
    const result = await Scamper.compileProgram(src).asyncAndThen(async prog =>
      Scamper.ok({
        statements: prog,
        outputs: await Scamper.evaluateProgram(prog, true)
      }))
    if (result.tag === 'error') {
      element.innerHTML = sanitize(Scamper.errorToString(result))
    } else {
      element.innerHTML = ''
      const endLocs = result.value.statements.map(s => s.range.end)
      const segments = Scamper.splitByLocs(endLocs, src)
      result.value.statements.forEach((s, i) => {
        const segment = segments[i]
        const outp = result.value.outputs[i]
        const outDiv = makeTag('div', ['class', 'output'])
        const progDiv = makeTag('div', ['class', 'program language-racket'])
        progDiv.appendChild(Pretty.makeCodeElt(segment + '\n'))
        if (!outputProg) {
          progDiv.setAttribute('style', 'display: none;')
        }
        // TODO: patching for now to observe output for values
        // will need to handle other non-value effects here
        if (outp.tag === 'value') {
          outDiv.appendChild(Pretty.valueToNode(outp.output))
        } else if (outp.tag === 'error') {
          outDiv.appendChild(document.createTextNode(
            outp.errors.map(Scamper.Result.detailsToCompleteString).join('\n')))
        } else if (outp.tag === 'testresult') {
          if (outp.passed) {
            outDiv.appendChild(document.createTextNode(`[[ Test "${outp.desc}" passed! ]]`))
          } else {
            const msg: string = outp.reason
              ? outp.reason
              : `  Expected: ${Scamper.expToString(0, outp.expected!)}\n  Actual: ${Scamper.expToString(0, outp.actual!)}`
            outDiv.appendChild(document.createTextNode(
              `[[ Test "${outp.desc}" failed!\n${msg}\n]]`))
          }
        }
        element.appendChild(progDiv)
        element.appendChild(outDiv)
      })
    }
    Prism.highlightAll()
  }
}

async function replaceExplorationWidgets () {
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
      element.innerHTML = sanitize(Scamper.errorToString(result))
      return
    } else {
      const trace = new Scamper.ProgramTrace(new Scamper.ProgramState(result.value))
      const update = async () => {
        await forEachByClass(element, 'step-counter', e => {
          e.innerHTML = `Step ${trace.currentStep()}`
          return Promise.resolve(undefined)
        })
        programElement.innerHTML = Scamper.progToString(0, trace.currentState().prog, true, true, '\n\n')
      }

      // Rig the buttons that are present in the widget
      await forEachByClass(element, 'step-forward', e => {
        (e as HTMLButtonElement).onclick = async () => { await trace.stepForward(); await update() }
        return Promise.resolve(undefined)
      })
      await forEachByClass(element, 'step-backward', e => {
        (e as HTMLButtonElement).onclick = async () => { trace.stepBackward(); await update() }
        return Promise.resolve(undefined)
      })
      await forEachByClass(element, 'stmt-forward', e => {
        (e as HTMLButtonElement).onclick = async () => { await trace.evalNextStmt(); await update() }
        return Promise.resolve(undefined)
      })
      await forEachByClass(element, 'stmt-backward', e => {
        (e as HTMLButtonElement).onclick = async () => { trace.revertPrevStmt(); await update() }
        return Promise.resolve(undefined)
      })
      await forEachByClass(element, 'eval', e => {
        (e as HTMLButtonElement).onclick = async () => { await trace.evaluateProg(); await update() }
        return Promise.resolve(undefined)
      })
      await forEachByClass(element, 'reset', e => {
        (e as HTMLButtonElement).onclick = async () => { trace.resetProg(); await update() }
        return Promise.resolve(undefined)
      })
      await forEachByClass(element, 'add-statement', e => {
        (e as HTMLButtonElement).onclick = async () => {
          // N.B., should really be an id, but the need to give the input a globally
          // unique id!
          const inputs = document.getElementsByClassName('add-statement-input')
          if (inputs.length > 0) {
            const input = inputs[0] as HTMLInputElement
            if (input.value.length > 0) {
              await trace.addStmt(input.value)
              await trace.evaluateProg()
              input.value = ''
              await update()
            }
          }
          return Promise.resolve(undefined)
        }
        return Promise.resolve(undefined)
      })
      // N.B., after setting up the panel, update the program panel to reflect
      // the initial state of the program.
      await update()
    }
  }
}

global.replaceCodeWidgets = async function () {
  await replaceOutputWidgets()
  await replaceExplorationWidgets()
}

global.ok = Scamper.ok
global.error = Scamper.error
