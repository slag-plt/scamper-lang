import { runtimeError } from '../runtime.js'
import { ok } from '../result.js'
import * as E from '../evaluator.js'
import * as L from '../lang.js'
import * as Utils from './utils.js'

const textAreaDoc: L.Doc = new L.Doc(
  '(text-area id) -> text-area?', [
    'id: string?'
  ],
  'Creates a text area with the given id.'
)

const textAreaPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('text-area', ['string?'], undefined, args, app).andThen(_ => {
    const ret = new HTMLTextAreaElement()
    ret.id = args[0] as string
    return ok(ret)
  }))

const textAreaGetDoc: L.Doc = new L.Doc(
  '(text-area-get text-area): string?', [
    'text-area: text-area?'
  ],
  'Returns the text in the given text area.'
)

const textAreaGetPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('text-area-get', ['any'], undefined, args, app).andThen(_ => {
    const textarea = args[0] as HTMLTextAreaElement
    return ok(textarea.textContent)
  }))

const buttonDoc: L.Doc = new L.Doc(
  '(button id label) -> button?', [
    'id: string?',
    'label: string?'
  ],
  'Creates a button with the given id and label.'
)

const buttonPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('button', ['string?', 'string?'], undefined, args, app).andThen(_ => {
    const ret = document.createElement('button')
    ret.id = args[0] as string
    ret.textContent = args[1] as string
    return ok(ret)
  }))

const buttonOnclickDoc: L.Doc = new L.Doc(
  '(button-onclick button f) -> button?', [
    'button: button?',
    'f: procedure?'
  ],
  'Sets the function that is called when the button is pressed to be `f`.'
)

const buttonOnclick: L.Prim = (env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('button-onclick', ['any', 'procedure?'], undefined, args, app).andThen(_ => {
    const button = args[0] as HTMLButtonElement
    const fn = args[1] as L.FunctionType
    button.onclick = async () => {
      await E.evaluateExp(env, L.nlecall(L.nlevalue(fn), []))
    }
    return ok(undefined)
  }))

const outputAreaDoc: L.Doc = new L.Doc(
  '(output-area id text) -> output-area?', [
    'id: string?',
    'text: string?'
  ],
  'Creates an output area with the given id and initial text.'
)

const outputAreaPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('output-area', ['string?', 'string?'], undefined, args, app).andThen(_ => {
    const ret = document.createElement('div')
    ret.id = args[0] as string
    ret.textContent = args[1] as string
    return ok(ret)
  }))

const outputAreaPutDoc: L.Doc = new L.Doc(
  '(output-area-put output-area text) -> output-area?', [
    'output-area: output-area?',
    'text: string?'
  ],
  'Sets the text in the given output area to be `text`.'
)

const outputAreaPutPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('output-area-put', ['any', 'string?'], undefined, args, app).andThen(_ => {
    const div = args[0] as Element
    div.textContent = args[1] as string
    return ok(undefined)
  }))

const tagDoc: L.Doc = new L.Doc(
  '(tag name c1 c2...) -> element?', [
    'name: string?',
    'c: any'
  ],
  'Creates an HTML element with the given name and children.'
)

const tagPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('tag', ['string?'], 'any', args, app).andThen(_ => {
    const elt = document.createElement(args[0] as string)
    if (args.length > 1 && L.valueIsList(args[1])) {
      const attrs = L.valueListToArray_(args[1])
      for (const attr of attrs) {
        if (L.valueIsPair(attr)) {
          const pair = attr as L.PairType
          if (!L.valueIsString(pair.fst)) {
            return runtimeError(`attribute must be a string: ${L.valueToString(pair.fst)}`)
          } else if (!L.valueIsString(pair.snd)) {
            return runtimeError(`attribute value must be a string: ${L.valueToString(pair.snd)}`)
          } else {
            elt.setAttribute(pair.fst as string, pair.snd as string)
          }
        }
      }
      // N.B., slice off the head and attribute list to obtain just children
      args = args.slice(2)
    } else {
      // N.B., slice off just the head to obtain the children
      args = args.slice(1)
    }
    for (const child of args) {
      if (child instanceof Element) {
        elt.appendChild(child)
      } else {
        elt.textContent = child as string
      }
    }
    return ok(elt)
  }))

const htmlEntry = (prim: L.Prim, docs?: L.Doc) => L.entry(L.vprim(prim), 'html', undefined, docs)

export const htmlLib: L.Env = new L.Env([
  ['text-area', htmlEntry(textAreaPrim, textAreaDoc)],
  ['text-area-get', htmlEntry(textAreaGetPrim, textAreaGetDoc)],
  ['button', htmlEntry(buttonPrim, buttonDoc)],
  ['button-onclick', htmlEntry(buttonOnclick, buttonOnclickDoc)],
  ['output-area', htmlEntry(outputAreaPrim, outputAreaDoc)],
  ['output-area-put', htmlEntry(outputAreaPutPrim, outputAreaPutDoc)],
  ['tag', htmlEntry(tagPrim, tagDoc)]
])
