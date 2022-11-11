import { ok } from '../result.js'
import { runtimeError } from '../runtime.js'
import * as L from '../lang.js'
import * as Pretty from '../pretty.js'
import * as Utils from './utils.js'
import { msg } from '../messages.js'

const jsNewDoc: L.Doc = new L.Doc(
  '(js-new class v1 v2 ...)', [
    'class: string?',
    'v: any?'
  ],
  'Creates a new instance of the given class. The class must be declared in the global scope.'
)

const jsNewPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('js-new', ['string?'], 'any', args, app).andThen(_ => {
    const className = args[0] as string
    const ctorArgs = args.slice(1) as any[]
    // eslint-disable-next-line no-eval, @typescript-eslint/no-unsafe-return, @typescript-eslint/no-unsafe-call
    return new (eval(className))(...ctorArgs)
  }))

const jsObjDoc: L.Doc = new L.Doc(
  '(js-obj k1 v1 k2 v2 ...): boolean?', [
    'k: string?',
    'v: any?'
  ],
  'Returns a raw JS object composed of the given key-value pairs.'
)

const jsObjPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('js-obj', [], 'any', args, app).andThen(_ => {
    if (args.length % 2 !== 0) {
      return runtimeError(msg('error-js-obj-args'), app)
    } else {
      const ret: any = {}
      for (let i = 0; i < args.length; i += 2) {
        if (!L.valueIsString(args[i])) {
          return runtimeError(msg('error-precondition-not-met', 'js-obj', i, 'string', Pretty.valueToString(0, args[i])), app)
        } else {
          // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
          ret[args[i] as string] = args[i + 1]
        }
      }
      return ok(ret)
    }
  }))

const jsGetDoc: L.Doc = new L.Doc(
  '(js-get obj k): any?', [
    'obj: any?',
    'k: string?'
  ],
  'Returns the value of the given key in the given JS object.'
)

const jsGetPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('js-get', ['any', 'string?'], undefined, args, app).andThen(_ =>
    // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
    ok((args[0] as any)[args[1] as string])))

const jsCallDoc: L.Doc = new L.Doc(
  '(js-call f x1 x2 ...): any?', [
    'f: any?',
    'x: any?'
  ],
  'Calls the given javascript function with the given arguments.'
)

const jsCallPrim: L.Prim = (env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('js-call', ['any'], 'any', args, app).andThen(_ => {
    if (typeof args[0] !== 'function') {
      return runtimeError(msg('error-precondition-not-met', 'js-call', 1, 'function', Pretty.valueToString(0, args[0])))
    } else {
      return ok((args[0])(...args.slice(1)))
    }
  }))

const jsMethodDoc: L.Doc = new L.Doc(
  '(js-method obj f x1 x2...): any?', [
    'obj: any?',
    'f: string?',
    'x: any?'
  ],
  'Calls the given javascript method with the given receiver and arguments.'
)

const jsMethodPrim: L.Prim = (env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('js-method', ['any', 'string?'], 'any', args, app).andThen(_ => {
    // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
    const obj = args[0] as any
    const f = args[1] as string
    const rest = args.slice(2)
    // eslint-disable-next-line no-useless-call, @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-unsafe-call
    return ok(obj[f].call(obj, ...rest))
  }))

const jsEvalDoc: L.Doc = new L.Doc(
  '(js-eval src): any?', [
    'src: string?'
  ],
  'Evaluates the given javascript source code provided as a string, returning the result as a raw JS value.'
)

const jsEvalPrim: L.Prim = (env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('js-eval', ['string?'], undefined, args, app).andThen(_ => {
    // eslint-disable-next-line no-eval
    return ok(eval(args[0] as string))
  }))

const jsEntry = (prim: L.Prim, docs?: L.Doc) => L.entry(L.vprim(prim), 'js', undefined, docs)

export const jsLib: L.Env = new L.Env([
  ['js-obj', jsEntry(jsObjPrim, jsObjDoc)],
  ['js-new', jsEntry(jsNewPrim, jsNewDoc)],
  ['js-get', jsEntry(jsGetPrim, jsGetDoc)],
  ['js-call', jsEntry(jsCallPrim, jsCallDoc)],
  ['js-method', jsEntry(jsMethodPrim, jsMethodDoc)],
  ['js-eval', jsEntry(jsEvalPrim, jsEvalDoc)]
])
