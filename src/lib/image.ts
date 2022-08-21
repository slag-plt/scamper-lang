import { ok } from '../result.js'
import { runtimeError } from '../runtime.js'
import { Env, entry, asNum_, asString_, EObj, Exp, isInteger, isString, nleobj, nleprim, Prim, Doc } from '../lang.js'
import { msg } from '../messages.js'
import * as Utils from './utils.js'
import * as Docs from './docs.js'

type Mode = 'solid' | 'outline'

/* eslint-disable no-use-before-define */
type Drawing = Circle | Rectangle | Beside | Above | Overlay

type Circle = { tag: 'circle', width: number, height: number, radius: number, mode: Mode, color: string }
const circle = (radius: number, mode: Mode, color: string): Circle => ({
  tag: 'circle',
  width: 2 * radius,
  height: 2 * radius,
  radius,
  mode,
  color
})

function isDrawing (e: Exp): boolean {
  return e.tag === 'obj' && e.kind === 'Drawing'
}

const circlePrim: Prim = (_env, args, app) => {
  const argErr = Utils.checkArgs('circle', ['number?', 'string?', 'string?'], undefined, args, app)
  if (argErr) { return argErr }
  const radius = asNum_(args[0])
  const mode = asString_(args[1])
  const color = asString_(args[2])
  if (mode !== 'solid' && mode !== 'outline') {
    return runtimeError(msg('error-precondition-not-met', 'circle', '2', '"solid" or "outline"', mode), app)
  } else {
    return ok(nleobj('Drawing', circle(radius, mode, color)))
  }
}

type Rectangle = { tag: 'rectangle', width: number, height: number, mode: Mode, color: string }
const rectangle = (width: number, height: number, mode: Mode, color: string): Rectangle =>
  ({ tag: 'rectangle', width, height, mode, color })

const rectanglePrim: Prim = (_env, args, app) => {
  const argErr = Utils.checkArgs('rectangle', ['number?', 'number?', 'string?', 'string?'], undefined, args, app)
  if (argErr) { return argErr }
  const width = asNum_(args[0])
  const height = asNum_(args[1])
  const mode = asString_(args[2])
  const color = asString_(args[3])
  if (mode !== 'solid' && mode !== 'outline') {
    return runtimeError(msg('error-precondition-not-met', 'rectangle', '3', '"solid" or "outline"', mode), app)
  } else {
    return ok(nleobj('Drawing', rectangle(width, height, mode, color)))
  }
}

type Beside = { tag: 'beside', width: number, height: number, drawings: Drawing[] }
const beside = (drawings: Drawing[]): Beside => ({
  tag: 'beside',
  width: drawings.reduce((acc, d) => acc + d.width, 0),
  height: drawings.reduce((acc, d) => Math.max(acc, d.height), 0),
  drawings
})

const besidePrim: Prim = (_env, args, app) => {
  const argErr = Utils.checkArgs('beside', [], 'Drawing', args, app)
  if (argErr) { return argErr }
  return ok(nleobj('Drawing', beside(args.map(e => (e as EObj).obj as Drawing))))
}

type Above = { tag: 'above', width: number, height: number, drawings: Drawing[] }
const above = (drawings: Drawing[]): Above => ({
  tag: 'above',
  width: drawings.reduce((acc, d) => Math.max(acc, d.width), 0),
  height: drawings.reduce((acc, d) => acc + d.height, 0),
  drawings
})

const abovePrim: Prim = (_env, args, app) => {
  const argErr = Utils.checkArgs('above', [], 'Drawing', args, app)
  if (argErr) { return argErr }
  return ok(nleobj('Drawing', above(args.map(e => (e as EObj).obj as Drawing))))
}

type Overlay = { tag: 'overlay', width: number, height: number, drawings: Drawing[] }
const overlay = (drawings: Drawing[]): Overlay => ({
  tag: 'overlay',
  width: drawings.reduce((acc, d) => Math.max(acc, d.width), 0),
  height: drawings.reduce((acc, d) => Math.max(acc, d.height), 0),
  drawings
})

const overlayPrim: Prim = (_env, args, app) => {
  const argErr = Utils.checkArgs('overlay', [], 'Drawing', args, app)
  if (argErr) { return argErr }
  return ok(nleobj('Drawing', overlay(args.map(e => (e as EObj).obj as Drawing))))
}

const imageEntry = (prim: Prim, docs?: Doc) => entry(nleprim(prim), 'image', undefined, docs)

export const imageLib: Env = new Env([
  ['circle', imageEntry(circlePrim, Docs.circle)],
  ['rectangle', imageEntry(rectanglePrim, Docs.rectangle)],
  ['beside', imageEntry(besidePrim, Docs.beside)],
  ['above', imageEntry(abovePrim, Docs.above)],
  ['overlay', imageEntry(overlayPrim, Docs.overlay)]
])
