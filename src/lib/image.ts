import { join, ok, Result } from '../result.js'
import { runtimeError } from '../runtime.js'
import { Env, entry, asNum_, asString_, EObj, Exp, isInteger, isString, nleobj, nleprim, Prim, Doc, nlestr, asList_, isPair, asPair_, isNumber } from '../lang.js'
import { msg } from '../messages.js'
import * as Utils from './utils.js'
import * as Docs from './docs.js'
import { dirname } from 'path'

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
export type Drawing = Ellipse | Rectangle | Triangle | Path | Beside | Above | Overlay | OverlayOffset | Rotate | WithDash

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

type Path = { tag: 'path', width: number, height: number, points: [number, number][], mode: Mode, color: string }
const path = (width: number, height: number, points: [number, number][], mode: Mode, color: string) =>
  ({ tag: 'path', width, height, points, mode, color })

const pathPrim: Prim = (_env, args, app) => {
  const argErr = Utils.checkArgs('path', ['number?', 'number?', 'list?', 'string?', 'string?'], undefined, args, app)
  if (argErr) { return argErr }
  const width = asNum_(args[0])
  const height = asNum_(args[1])
  const mode = asString_(args[3])
  const color = asString_(args[4])
  if (mode !== 'solid' && mode !== 'outline') {
    return runtimeError(msg('error-precondition-not-met', 'path', '2', '"solid" or "outline"', mode), app)
  } else {
    const result: Result<[number,number][]> = join(asList_(args[2]).map(e => {
      if (isPair(e)) {
        const p = asPair_(e)
        if (!isNumber(p[0]) || !isNumber(p[1])) {
          return runtimeError(msg('error-type-expected-fun', 'path', 'list of pairs of numbers', e.tag), e)
        } 
        return ok([asNum_(p[0]), asNum_(p[1])])
      } else {
        return runtimeError(msg('error-type-expected-fun', 'path', 'list of pairs of numbers', e.tag), e)
      }
    }))
    return result.andThen((points: [number, number][]) =>
      ok(nleobj('Drawing', path(width, height, points, mode, color))))
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

type Above = { tag: 'above', align: string, width: number, height: number, drawings: Drawing[] }
const above = (align: string, drawings: Drawing[]): Above => ({
  tag: 'above',
  align,
  width: Math.max(...drawings.map(d => d.width)),
  height: drawings.reduce((acc, d) => acc + d.height, 0),
  drawings
})

const abovePrim: Prim = (_env, args, app) => {
  const argErr = Utils.checkArgs('above', [], 'Drawing', args, app)
  if (argErr) { return argErr }
  return ok(nleobj('Drawing', above('middle', args.map(e => (e as EObj).obj as Drawing))))
}

const aboveAlignPrim: Prim = (_env, args, app) => {
  const argErr = Utils.checkArgs('above-align', ['string?'], 'Drawing', args, app)
  if (argErr) { return argErr }
  const align = asString_(args[0])
  if (align !== 'left' && align !== 'middle' && align !== 'right') {
    return runtimeError(msg('error-precondition-not-met', 'above-align', '1', '"left", "middle", or "right"', align), app)
  } else {
    return ok(nleobj('Drawing', above(align, args.slice(1).map(e => (e as EObj).obj as Drawing))))
  }
}

type Overlay = { tag: 'overlay', xAlign: string, yAlign: string, width: number, height: number, drawings: Drawing[] }
const overlay = (xAlign: string, yAlign: string, drawings: Drawing[]): Overlay => ({
  tag: 'overlay',
  xAlign,
  yAlign,
  width: Math.max(...drawings.map(d => d.width)),
  height: Math.max(...drawings.map(d => d.height)),
  drawings
})

const overlayPrim: Prim = (_env, args, app) => {
  const argErr = Utils.checkArgs('overlay', [], 'Drawing', args, app)
  if (argErr) { return argErr }
  return ok(nleobj('Drawing', overlay('middle', 'center', args.map(e => (e as EObj).obj as Drawing))))
}

const overlayAlignPrim: Prim = (_env, args, app) => {
  const argErr = Utils.checkArgs('overlay-align', ['string?', 'string?'], 'Drawing', args, app)
  if (argErr) { return argErr }
  const xAlign = asString_(args[0])
  const yAlign = asString_(args[1])
  if (xAlign !== 'left' && xAlign !== 'middle' && xAlign !== 'right') {
    return runtimeError(msg('error-precondition-not-met', 'overlay-align', '1', '"left", "middle", or "right"', xAlign), app)
  } else if (yAlign !== 'top' && yAlign !== 'center' && yAlign !== 'bottom') {
    return runtimeError(msg('error-precondition-not-met', 'overlay-align', '2', '"top", "center", or "bottom"', yAlign), app)
  } else {
    return ok(nleobj('Drawing', overlay(xAlign, yAlign, args.slice(2).map(e => (e as EObj).obj as Drawing))))
  }
}

type OverlayOffset = { tag: 'overlayOffset', dx: number, dy: number, width: number, height: number, d1: Drawing, d2: Drawing }
const overlayOffset = (dx: number, dy: number, d1: Drawing, d2: Drawing) => {
  // N.B., tricky! Need to account for whether (a) we are shifting the smaller
  // or larger image and (b) whether we are shifting it positively or
  // negatively.
  let width;
  if (d1.width >= d2.width) {
    width = dx >= 0
      ? Math.max(d1.width, d2.width + Math.abs(dx))
      : Math.abs(dx) + d1.width
  } else {
    width = dx <= 0
      ? Math.max(d2.width, d1.width + Math.abs(dx))
      : Math.abs(dx) + d2.width
  }
  let height;
  if (d1.height >= d2.height) {
    height = dy >= 0
      ? Math.max(d1.height, d2.height + Math.abs(dy))
      : Math.abs(dy) + d1.height
  } else {
    height = dy <= 0
      ? Math.max(d2.height, d1.height + Math.abs(dy))
      : Math.abs(dy) + d2.height
  }
  return {
    tag: 'overlayOffset',
    dx,
    dy,
    // BUG: what if d2 is actually bigger than d1? Then the calculation needs to mirror!
    width,
    height,
    d1,
    d2
  }
}

const overlayOffsetPrim: Prim = (_env, args, app) => {
  const argErr = Utils.checkArgs('overlay-offset', ['Drawing', 'number?', 'number?', 'Drawing'], undefined, args, app)
  if (argErr) { return argErr }
  const dx = asNum_(args[1])
  const dy = asNum_(args[2])
  return ok(nleobj('Drawing', overlayOffset(dx, dy, (args[0] as EObj).obj as Drawing, (args[3] as EObj).obj as Drawing)))
}

type Rotate = { tag: 'rotate', width: number, height: number, angle: number, drawing: Drawing }
// const rotate = (angle: number, drawing: Drawing): Rotate => ({
//   tag: 'rotate',
//   width: drawing.width * Math.abs(Math.cos(angle * Math.PI / 180)) + drawing.height * Math.abs(Math.sin(angle * Math.PI / 180)),
//   height: drawing.width * Math.abs(Math.sin(angle * Math.PI / 180)) + drawing.height * Math.abs(Math.cos(angle * Math.PI /180)),
//   angle,
//   drawing
// })

function calculateRotatedBox(width: number, height: number, degrees: number): { width: number, height: number } {
  // Calculate the rotated corners of the box
  const angle = degrees * Math.PI / 180
  const origPoints = [
    [-width / 2, -height / 2],
    [width / 2, -height / 2],
    [-width / 2, height / 2],
    [width / 2, height / 2]
  ]
  const rotatedPoints = origPoints.map(
    ([x, y]) => [
      x * Math.cos(angle) - y * Math.sin(angle),
      x * Math.sin(angle) + y * Math.cos(angle)
    ]
  )

  // Determine the width and height of the box's bounding
  // box by taking mins and maxes of the points.
  const xMin = Math.min(...rotatedPoints.map(([x, _]) => x))
  const xMax = Math.max(...rotatedPoints.map(([x, _]) => x))
  const yMin = Math.min(...rotatedPoints.map(([_, y]) => y))
  const yMax = Math.max(...rotatedPoints.map(([_, y]) => y))

  return {
    width: xMax - xMin,
    height: yMax - yMin
  }
}

const rotate = (angle: number, drawing: Drawing): Rotate => {
  const dims = calculateRotatedBox(drawing.width, drawing.height, angle)
  return {
    tag: 'rotate',
    width: dims.width,
    height: dims.height,
    angle,
    drawing
  }
}

const rotatePrim: Prim = (_env, args, app) => {
  const argErr = Utils.checkArgs('rotate', ['number?', 'Drawing'], undefined, args, app)
  if (argErr) { return argErr }
  const angle = asNum_(args[0])
  return ok(nleobj('Drawing', rotate(angle, (args[1] as EObj).obj as Drawing)))
}

type WithDash = { tag: 'withDash', dashSpec: number[], drawing: Drawing, width: number, height: number }
const withDash = (dashSpec: number[], drawing: Drawing): WithDash => ({
  tag: 'withDash',
  dashSpec,
  drawing,
  width: drawing.width,
  height: drawing.height
})

const withDashPrim: Prim = (_env, args, app) => {
  const argErr = Utils.checkArgs('with-dash', ['list?', 'Drawing'], undefined, args, app)
  if (argErr) { return argErr }
  const es = asList_(args[0])
  for (const e of es) {
    if (!isNumber(e)) {
      runtimeError(msg('error-precondition-not-met', 'with-dash', '1', 'list of numbers', es), app)
    }
  }
  const dashes = es.map(e => asNum_(e))
  return ok(nleobj('Drawing', withDash(dashes, (args[1] as EObj).obj as Drawing)))
}

const imageEntry = (prim: Prim, docs?: Doc) => entry(nleprim(prim), 'image', undefined, docs)

export const imageLib: Env = new Env([
  ['color', imageEntry(colorPrim, Docs.color)],
  ['ellipse', imageEntry(ellipsePrim, Docs.ellipse)],
  ['circle', imageEntry(circlePrim, Docs.circle)],
  ['rectangle', imageEntry(rectanglePrim, Docs.rectangle)],
  ['square', imageEntry(squarePrim, Docs.drawingSquare)],
  ['triangle', imageEntry(trianglePrim, Docs.triangle)],
  ['path', imageEntry(pathPrim, Docs.path)],
  ['beside', imageEntry(besidePrim, Docs.beside)],
  ['beside/align', imageEntry(besideAlignPrim, Docs.besideAlign)],
  ['above', imageEntry(abovePrim, Docs.above)],
  ['above/align', imageEntry(aboveAlignPrim, Docs.aboveAlign)],
  ['overlay', imageEntry(overlayPrim, Docs.overlay)],
  ['overlay/align', imageEntry(overlayAlignPrim, Docs.overlayAlign)],
  ['overlay/offset', imageEntry(overlayOffsetPrim, Docs.overlayOffset)],
  ['rotate', imageEntry(rotatePrim, Docs.rotate)],
  ['with-dashes', imageEntry(withDashPrim, Docs.withDashes)],
])
