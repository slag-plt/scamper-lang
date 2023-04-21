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
  '(button label fn) -> button?', [
    'label: string?',
    'fn: procedure?'
  ],
  'Creates a button with the given label and function that is called when the button is pressed.'
)

const buttonPrim: L.Prim = (env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('button', ['string?', 'procedure?'], undefined, args, app).andThen(_ => {
    const ret = document.createElement('button')
    ret.textContent = args[0] as string
    const fn = args[1] as L.FunctionType
    ret.onclick = async () => {
      await E.evaluateExp(env, L.nlecall(L.nlevalue(fn), []))
    }
    return ok(ret)
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

const tagSetChildrenDoc: L.Doc = new L.Doc(
  '(tag-set-children! name c1 c2...) -> element?', [
    'elt: an HTML element',
    'c: an HTML element or string'
  ],
  'Sets `elt`\'s children to be `c1`, `c2`, ..'
)

const tagSetChildrenPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('tag-set-children!', ['any'], 'any', args, app).andThen(_ => {
    const elt = args[0] as Element
    if (!(elt instanceof HTMLElement)) {
      return runtimeError(`tag-set-children! expects an HTML element, but received ${L.valueToString(elt)}`)
    } else {
      const children = args.slice(1)
      for (const child of children) {
        if (child instanceof Element) {
          elt.appendChild(child)
        } else {
          elt.textContent = child as string
        }
      }
      return ok(undefined)
    }
  }))

const ignoreDoc: L.Doc = new L.Doc(
  '(ignore v) -> element?', [
    'e: any'
  ],
  'Surpresses the output of `v` to the page.'
)

const ignorePrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('ignore', ['any'], undefined, args, app).andThen(_ => {
    const ret = document.createElement('div')
    return ok(ret)
  }))

const htmlEntry = (prim: L.Prim, docs?: L.Doc) => L.entry(L.vprim(prim), 'html', undefined, docs)

export const htmlLib: L.Env = new L.Env([
  ['text-area', htmlEntry(textAreaPrim, textAreaDoc)],
  ['text-area-get', htmlEntry(textAreaGetPrim, textAreaGetDoc)],
  ['button', htmlEntry(buttonPrim, buttonDoc)],
  ['tag', htmlEntry(tagPrim, tagDoc)],
  ['tag-set-children!', htmlEntry(tagSetChildrenPrim, tagSetChildrenDoc)],
  ['ignore', htmlEntry(ignorePrim, ignoreDoc)]
])
