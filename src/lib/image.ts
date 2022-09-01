import { ok } from '../result.js'
import { runtimeError } from '../runtime.js'
import { Env, entry, asNum_, asString_, EObj, Exp, isInteger, isString, nleobj, nleprim, Prim, Doc, nlestr } from '../lang.js'
import { msg } from '../messages.js'
import * as Utils from './utils.js'
import * as Docs from './docs.js'

type Color = { tag: 'color', r: number, g: number, b: number, a: number }
const color = (r: number, g: number, b: number, a: number): Color => ({ tag: 'color', r, g, b, a })

const colorPrim: Prim = (_env, args, app) =>
  Utils.checkArgsResult('color', ['number?', 'number?', 'number?', 'number?'], undefined, args, app).andThen(_ => {
    const r = asNum_(args[0])
    const g = asNum_(args[1])
    const b = asNum_(args[2])
    const a = asNum_(args[3])
    const isValid = (n: number) => n >= 0 && n <= 255
    if (!isValid(r)) {
      return runtimeError(msg(
        'error-precondition-not-met',
        'color',
        1,
        'a number in the range 0--255',
        r), app)
    } else if (!isValid(g)) {
      return runtimeError(msg(
        'error-precondition-not-met',
        'color',
        2,
        'a number in the range 0--255',
        g), app)
    } else if (!isValid(b)) {
      return runtimeError(msg(
        'error-precondition-not-met',
        'color',
        3,
        'a number in the range 0--255',
        b), app)
    } else if (!(a >= 0 && a <= 1)) {
      return runtimeError(msg(
        'error-precondition-not-met',
        'color',
        4,
        'a number in the range 0--1',
        a), app)
    } else {
      return ok(nlestr(`rgba(${asNum_(args[0])}, ${asNum_(args[1])}, ${asNum_(args[2])}, ${asNum_(args[3])})`))
    }
  })

type Mode = 'solid' | 'outline'

/* eslint-disable no-use-before-define */
export type Drawing = Ellipse | Rectangle | Triangle | Beside | Above | Overlay | Rotate

type Ellipse = { tag: 'ellipse', width: number, height: number, mode: Mode, color: string }
const ellipse = (width: number, height: number, mode: Mode, color: string): Ellipse => ({
  tag: 'ellipse',
  width,
  height,
  mode,
  color
})

function isDrawing (e: Exp): boolean {
  return e.tag === 'obj' && e.kind === 'Drawing'
}

const ellipsePrim: Prim = (_env, args, app) => {
  const argErr = Utils.checkArgs('ellipse', ['number?', 'number?', 'string?', 'string?'], undefined, args, app)
  if (argErr) { return argErr }
  const width = asNum_(args[0])
  const height = asNum_(args[1])
  const mode = asString_(args[2])
  const color = asString_(args[3])
  if (mode !== 'solid' && mode !== 'outline') {
    return runtimeError(msg('error-precondition-not-met', 'circle', '3', '"solid" or "outline"', mode), app)
  } else {
    return ok(nleobj('Drawing', ellipse(width, height, mode, color)))
  }
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
    return ok(nleobj('Drawing', ellipse(radius * 2, radius * 2, mode, color)))
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

const squarePrim: Prim = (_env, args, app) => {
  const argErr = Utils.checkArgs('square', ['number?', 'string?', 'string?'], undefined, args, app)
  if (argErr) { return argErr }
  const width = asNum_(args[0])
  const mode = asString_(args[1])
  const color = asString_(args[2])
  if (mode !== 'solid' && mode !== 'outline') {
    return runtimeError(msg('error-precondition-not-met', 'square', '2', '"solid" or "outline"', mode), app)
  } else {
    return ok(nleobj('Drawing', rectangle(width, width, mode, color)))
  }
}

type Triangle = { tag: 'triangle', width: number, height: number, length: number, mode: Mode, color: string }
const triangle = (length: number, mode: Mode, color: string): Triangle => ({
  tag: 'triangle', 
  width: length,
  height: length * Math.sqrt(3) / 2,
  length,
  mode,
  color
})

const trianglePrim: Prim = (_env, args, app) => {
  const argErr = Utils.checkArgs('triangle', ['number?', 'string?', 'string?'], undefined, args, app)
  if (argErr) { return argErr }
  const length = asNum_(args[0])
  const mode = asString_(args[1])
  const color = asString_(args[2])
  if (mode !== 'solid' && mode !== 'outline') {
    return runtimeError(msg('error-precondition-not-met', 'triangle', '2', '"solid" or "outline"', mode), app)
  } else {
    return ok(nleobj('Drawing', triangle(length, mode, color)))
  }
}

type Beside = { tag: 'beside', align: string, width: number, height: number, drawings: Drawing[] }
const beside = (align: string, drawings: Drawing[]): Beside => ({
  tag: 'beside',
  align,
  width: drawings.reduce((acc, d) => acc + d.width, 0),
  height: Math.max(...drawings.map(d => d.height)),
  drawings
})

const besidePrim: Prim = (_env, args, app) => {
  const argErr = Utils.checkArgs('beside', [], 'Drawing', args, app)
  if (argErr) { return argErr }
  return ok(nleobj('Drawing', beside('center', args.map(e => (e as EObj).obj as Drawing))))
}

const besideAlignPrim: Prim = (_env, args, app) => {
  const argErr = Utils.checkArgs('beside-align', ['string?'], 'Drawing', args, app)
  if (argErr) { return argErr }
  const align = asString_(args[0])
  if (align !== 'top' && align !== 'center' && align !== 'bottom') {
    return runtimeError(msg('error-precondition-not-met', 'beside-align', '1', '"top", "center", or "bottom"', align), app)
  } else {
    return ok(nleobj('Drawing', beside(align, args.slice(1).map(e => (e as EObj).obj as Drawing))))
  }
}

type Above = { tag: 'above', width: number, height: number, drawings: Drawing[] }
const above = (drawings: Drawing[]): Above => ({
  tag: 'above',
  width: Math.max(...drawings.map(d => d.width)),
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
  width: Math.max(...drawings.map(d => d.width)),
  height: Math.max(...drawings.map(d => d.height)),
  drawings
})

const overlayPrim: Prim = (_env, args, app) => {
  const argErr = Utils.checkArgs('overlay', [], 'Drawing', args, app)
  if (argErr) { return argErr }
  return ok(nleobj('Drawing', overlay(args.map(e => (e as EObj).obj as Drawing))))
}

type Rotate = { tag: 'rotate', width: number, height: number, angle: number, drawing: Drawing }
const rotate = (angle: number, drawing: Drawing): Rotate => ({
  tag: 'rotate',
  width: drawing.width * Math.abs(Math.cos(angle * Math.PI / 180)) + drawing.height * Math.abs(Math.sin(angle * Math.PI / 180)),
  height: drawing.width * Math.abs(Math.sin(angle * Math.PI / 180)) + drawing.height * Math.abs(Math.cos(angle * Math.PI /180)),
  angle,
  drawing
})

const rotatePrim: Prim = (_env, args, app) => {
  const argErr = Utils.checkArgs('rotate', ['number?', 'Drawing'], undefined, args, app)
  if (argErr) { return argErr }
  const angle = asNum_(args[0])
  return ok(nleobj('Drawing', rotate(angle, (args[1] as EObj).obj as Drawing)))
}

const imageEntry = (prim: Prim, docs?: Doc) => entry(nleprim(prim), 'image', undefined, docs)

export const imageLib: Env = new Env([
  ['color', imageEntry(colorPrim, Docs.color)],
  ['ellipse', imageEntry(ellipsePrim, Docs.ellipse)],
  ['circle', imageEntry(circlePrim, Docs.circle)],
  ['rectangle', imageEntry(rectanglePrim, Docs.rectangle)],
  ['square', imageEntry(squarePrim, Docs.drawingSquare)],
  ['triangle', imageEntry(trianglePrim, Docs.triangle)],
  ['beside', imageEntry(besidePrim, Docs.beside)],
  ['beside/align', imageEntry(besideAlignPrim, Docs.besideAlign)],
  ['above', imageEntry(abovePrim, Docs.above)],
  ['overlay', imageEntry(overlayPrim, Docs.overlay)]
  // ['rotate', imageEntry(rotatePrim, Docs.rotate)]
])
