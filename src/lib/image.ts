/* eslint-disable @typescript-eslint/restrict-template-expressions */
/* eslint-disable @typescript-eslint/no-unsafe-return, @typescript-eslint/no-unsafe-call */
import { join, ok, Result } from '../result.js'
import { runtimeError } from '../runtime.js'
import * as L from '../lang.js'
import { msg } from '../messages.js'
import * as Utils from './utils.js'
import * as Docs from './docs.js'

const colorPrim: L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('color', ['number?', 'number?', 'number?', 'number?'], undefined, args, app).asyncAndThen(_ => {
    const r = args[0] as number
    const g = args[1] as number
    const b = args[2] as number
    const a = args[3] as number
    const isValid = (n: number) => n >= 0 && n <= 255
    if (!isValid(r)) {
      return Promise.resolve(runtimeError(msg(
        'error-precondition-not-met',
        'color',
        1,
        'a number in the range 0--255',
        r), app))
    } else if (!isValid(g)) {
      return Promise.resolve(runtimeError(msg(
        'error-precondition-not-met',
        'color',
        2,
        'a number in the range 0--255',
        g), app))
    } else if (!isValid(b)) {
      return Promise.resolve(runtimeError(msg(
        'error-precondition-not-met',
        'color',
        3,
        'a number in the range 0--255',
        b), app))
    } else if (!(a >= 0 && a <= 1)) {
      return Promise.resolve(runtimeError(msg(
        'error-precondition-not-met',
        'color',
        4,
        'a number in the range 0--1',
        a), app))
    } else {
      return Promise.resolve(ok(`rgba(${args[0]}, ${args[1]}, ${args[2]}, ${args[3]})`))
    }
  })

type Mode = 'solid' | 'outline'

/* eslint-disable no-use-before-define */
export type Drawing = Ellipse | Rectangle | Triangle | Path | Beside | Above | Overlay | OverlayOffset | Rotate | WithDash

const imagePrim: L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('image?', ['any'], undefined, args, app).asyncAndThen(_ =>
    Promise.resolve(ok(L.valueHasPropertyValue(args[0], 'renderAs', 'drawing'))))

type Ellipse = { renderAs: 'drawing', tag: 'ellipse', width: number, height: number, mode: Mode, color: string }
const ellipse = (width: number, height: number, mode: Mode, color: string): Ellipse => ({
  renderAs: 'drawing',
  tag: 'ellipse',
  width,
  height,
  mode,
  color
})

const ellipsePrim: L.Prim = (_env, args, app) => {
  const argErr = Utils.checkArgs('ellipse', ['number?', 'number?', 'string?', 'string?'], undefined, args, app)
  if (argErr) { return Promise.resolve(argErr) }
  const width = args[0] as number
  const height = args[1] as number
  const mode = args[2] as string
  const color = args[3] as string
  if (mode !== 'solid' && mode !== 'outline') {
    return Promise.resolve(runtimeError(msg('error-precondition-not-met', 'circle', '3', '"solid" or "outline"', mode), app))
  } else {
    return Promise.resolve(ok(ellipse(width, height, mode, color)))
  }
}

const circlePrim: L.Prim = (_env, args, app) => {
  const argErr = Utils.checkArgs('circle', ['number?', 'string?', 'string?'], undefined, args, app)
  if (argErr) { return Promise.resolve(argErr) }
  const radius = args[0] as number
  const mode = args[1] as string
  const color = args[2] as string
  if (mode !== 'solid' && mode !== 'outline') {
    return Promise.resolve(runtimeError(msg('error-precondition-not-met', 'circle', '2', '"solid" or "outline"', mode), app))
  } else {
    return Promise.resolve(ok(ellipse(radius * 2, radius * 2, mode, color)))
  }
}

type Rectangle = { renderAs: 'drawing', tag: 'rectangle', width: number, height: number, mode: Mode, color: string }
const rectangle = (width: number, height: number, mode: Mode, color: string): Rectangle =>
  ({ renderAs: 'drawing', tag: 'rectangle', width, height, mode, color })

const rectanglePrim: L.Prim = (_env, args, app) => {
  const argErr = Utils.checkArgs('rectangle', ['number?', 'number?', 'string?', 'string?'], undefined, args, app)
  if (argErr) { return Promise.resolve(argErr) }
  const width = args[0] as number
  const height = args[1] as number
  const mode = args[2] as string
  const color = args[3] as string
  if (mode !== 'solid' && mode !== 'outline') {
    return Promise.resolve(runtimeError(msg('error-precondition-not-met', 'rectangle', '3', '"solid" or "outline"', mode), app))
  } else {
    return Promise.resolve(ok(rectangle(width, height, mode, color)))
  }
}

const squarePrim: L.Prim = (_env, args, app) => {
  const argErr = Utils.checkArgs('square', ['number?', 'string?', 'string?'], undefined, args, app)
  if (argErr) { return Promise.resolve(argErr) }
  const width = args[0] as number
  const mode = args[1] as string
  const color = args[2] as string
  if (mode !== 'solid' && mode !== 'outline') {
    return Promise.resolve(runtimeError(msg('error-precondition-not-met', 'square', '2', '"solid" or "outline"', mode), app))
  } else {
    return Promise.resolve(ok(rectangle(width, width, mode, color)))
  }
}

type Triangle = { renderAs: 'drawing', tag: 'triangle', width: number, height: number, length: number, mode: Mode, color: string }
const triangle = (length: number, mode: Mode, color: string): Triangle => ({
  renderAs: 'drawing',
  tag: 'triangle',
  width: length,
  height: length * Math.sqrt(3) / 2,
  length,
  mode,
  color
})

const trianglePrim: L.Prim = (_env, args, app) => {
  const argErr = Utils.checkArgs('triangle', ['number?', 'string?', 'string?'], undefined, args, app)
  if (argErr) { return Promise.resolve(argErr) }
  const length = args[0] as number
  const mode = args[1] as string
  const color = args[2] as string
  if (mode !== 'solid' && mode !== 'outline') {
    return Promise.resolve(runtimeError(msg('error-precondition-not-met', 'triangle', '2', '"solid" or "outline"', mode), app))
  } else {
    return Promise.resolve(ok(triangle(length, mode, color)))
  }
}

type Path = { renderAs: 'drawing', tag: 'path', width: number, height: number, points: [number, number][], mode: Mode, color: string }
const path = (width: number, height: number, points: [number, number][], mode: Mode, color: string) =>
  ({ renderAs: 'drawing', tag: 'path', width, height, points, mode, color })

const pathPrim: L.Prim = (_env, args, app) => {
  const argErr = Utils.checkArgs('path', ['number?', 'number?', 'list?', 'string?', 'string?'], undefined, args, app)
  if (argErr) { return Promise.resolve(argErr) }
  const width = args[0] as number
  const height = args[1] as number
  const mode = args[3] as string
  const color = args[4] as string
  if (mode !== 'solid' && mode !== 'outline') {
    return Promise.resolve(runtimeError(msg('error-precondition-not-met', 'path', '2', '"solid" or "outline"', mode), app))
  } else {
    const result: Result<[number, number][]> = join(L.valueListToArray_(args[2]).map(v => {
      if (L.valueIsPair(v)) {
        const p = v as L.PairType
        if (!L.valueIsNumber(p.fst) || !L.valueIsNumber(p.snd)) {
          return runtimeError(msg('error-type-expected-fun', 3, 'path', 'list of pairs of numbers', v), app)
        }
        return ok([p.fst as number, p.snd as number])
      } else {
        return runtimeError(msg('error-type-expected-fun', 3, 'path', 'list of pairs of numbers', v), app)
      }
    }))
    return Promise.resolve(result.andThen((points: [number, number][]) =>
      ok(path(width, height, points, mode, color))))
  }
}

type Beside = { renderAs: 'drawing', tag: 'beside', align: string, width: number, height: number, drawings: Drawing[] }
const beside = (align: string, drawings: Drawing[]): Beside => ({
  renderAs: 'drawing',
  tag: 'beside',
  align,
  width: drawings.reduce((acc, d) => acc + d.width, 0),
  height: Math.max(...drawings.map(d => d.height)),
  drawings
})

const besidePrim: L.Prim = (_env, args, app) => {
  const argErr = Utils.checkArgs('beside', [], 'drawing', args, app)
  if (argErr) { return Promise.resolve(argErr) }
  return Promise.resolve(ok(beside('center', args as Drawing[])))
}

const besideAlignPrim: L.Prim = (_env, args, app) => {
  const argErr = Utils.checkArgs('beside-align', ['string?'], 'drawing', args, app)
  if (argErr) { return Promise.resolve(argErr) }
  const align = args[0] as string
  if (align !== 'top' && align !== 'center' && align !== 'bottom') {
    return Promise.resolve(runtimeError(msg('error-precondition-not-met', 'beside-align', '1', '"top", "center", or "bottom"', align), app))
  } else {
    return Promise.resolve(ok(beside(align, args as Drawing[])))
  }
}

type Above = { renderAs: 'drawing', tag: 'above', align: string, width: number, height: number, drawings: Drawing[] }
const above = (align: string, drawings: Drawing[]): Above => ({
  renderAs: 'drawing',
  tag: 'above',
  align,
  width: Math.max(...drawings.map(d => d.width)),
  height: drawings.reduce((acc, d) => acc + d.height, 0),
  drawings
})

const abovePrim: L.Prim = (_env, args, app) => {
  const argErr = Utils.checkArgs('above', [], 'drawing', args, app)
  if (argErr) { return Promise.resolve(argErr) }
  return Promise.resolve(ok(above('middle', args as Drawing[])))
}

const aboveAlignPrim: L.Prim = (_env, args, app) => {
  const argErr = Utils.checkArgs('above-align', ['string?'], 'drawing', args, app)
  if (argErr) { return Promise.resolve(argErr) }
  const align = args[0] as string
  if (align !== 'left' && align !== 'middle' && align !== 'right') {
    return Promise.resolve(runtimeError(msg('error-precondition-not-met', 'above-align', '1', '"left", "middle", or "right"', align), app))
  } else {
    return Promise.resolve(ok(above(align, args.slice(1) as Drawing[])))
  }
}

type Overlay = { renderAs: 'drawing', tag: 'overlay', xAlign: string, yAlign: string, width: number, height: number, drawings: Drawing[] }
const overlay = (xAlign: string, yAlign: string, drawings: Drawing[]): Overlay => ({
  renderAs: 'drawing',
  tag: 'overlay',
  xAlign,
  yAlign,
  width: Math.max(...drawings.map(d => d.width)),
  height: Math.max(...drawings.map(d => d.height)),
  drawings
})

const overlayPrim: L.Prim = (_env, args, app) => {
  const argErr = Utils.checkArgs('overlay', [], 'drawing', args, app)
  if (argErr) { return Promise.resolve(argErr) }
  return Promise.resolve(ok(overlay('middle', 'center', args as Drawing[])))
}

const overlayAlignPrim: L.Prim = async (_env, args, app) => {
  const argErr = Utils.checkArgs('overlay-align', ['string?', 'string?'], 'drawing', args, app)
  if (argErr) { return Promise.resolve(argErr) }
  const xAlign = args[0] as string
  const yAlign = args[1] as string
  if (xAlign !== 'left' && xAlign !== 'middle' && xAlign !== 'right') {
    return Promise.resolve(runtimeError(msg('error-precondition-not-met', 'overlay-align', '1', '"left", "middle", or "right"', xAlign), app))
  } else if (yAlign !== 'top' && yAlign !== 'center' && yAlign !== 'bottom') {
    return Promise.resolve(runtimeError(msg('error-precondition-not-met', 'overlay-align', '2', '"top", "center", or "bottom"', yAlign), app))
  } else {
    return Promise.resolve(ok(overlay(xAlign, yAlign, args.slice(2) as Drawing[])))
  }
}

type OverlayOffset = { renderAs: 'drawing', tag: 'overlayOffset', dx: number, dy: number, width: number, height: number, d1: Drawing, d2: Drawing }
const overlayOffset = (dx: number, dy: number, d1: Drawing, d2: Drawing) => {
  // N.B., tricky! Need to account for whether (a) we are shifting the smaller
  // or larger image and (b) whether we are shifting it positively or
  // negatively.
  let width
  if (d1.width >= d2.width) {
    width = dx >= 0
      ? Math.max(d1.width, d2.width + Math.abs(dx))
      : Math.abs(dx) + d1.width
  } else {
    width = dx <= 0
      ? Math.max(d2.width, d1.width + Math.abs(dx))
      : Math.abs(dx) + d2.width
  }
  let height
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
    renderAs: 'drawing',
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

const overlayOffsetPrim: L.Prim = (_env, args, app) => {
  const argErr = Utils.checkArgs('overlay-offset', ['drawing', 'number?', 'number?', 'drawing'], undefined, args, app)
  if (argErr) { return Promise.resolve(argErr) }
  const dx = args[1] as number
  const dy = args[2] as number
  return Promise.resolve(ok(overlayOffset(dx, dy, args[0] as Drawing, args[3] as Drawing)))
}

type Rotate = { renderAs: 'drawing', tag: 'rotate', width: number, height: number, angle: number, drawing: Drawing }
// const rotate = (angle: number, drawing: Drawing): Rotate => ({
//   tag: 'rotate',
//   width: drawing.width * Math.abs(Math.cos(angle * Math.PI / 180)) + drawing.height * Math.abs(Math.sin(angle * Math.PI / 180)),
//   height: drawing.width * Math.abs(Math.sin(angle * Math.PI / 180)) + drawing.height * Math.abs(Math.cos(angle * Math.PI /180)),
//   angle,
//   drawing
// })

function calculateRotatedBox (width: number, height: number, degrees: number): { width: number, height: number } {
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
    renderAs: 'drawing',
    tag: 'rotate',
    width: dims.width,
    height: dims.height,
    angle,
    drawing
  }
}

const rotatePrim: L.Prim = (_env, args, app) => {
  const argErr = Utils.checkArgs('rotate', ['number?', 'drawing'], undefined, args, app)
  if (argErr) { return Promise.resolve(argErr) }
  const angle = args[0] as number
  return Promise.resolve(ok(rotate(angle, args[1] as Drawing)))
}

type WithDash = { renderAs: 'drawing', tag: 'withDash', dashSpec: number[], drawing: Drawing, width: number, height: number }
const withDash = (dashSpec: number[], drawing: Drawing): WithDash => ({
  renderAs: 'drawing',
  tag: 'withDash',
  dashSpec,
  drawing,
  width: drawing.width,
  height: drawing.height
})

const withDashPrim: L.Prim = (_env, args, app) => {
  const argErr = Utils.checkArgs('with-dash', ['list?', 'drawing'], undefined, args, app)
  if (argErr) { return Promise.resolve(argErr) }
  const es = L.valueListToArray_(args[0])
  for (const e of es) {
    if (!L.valueIsNumber(e)) {
      return Promise.resolve(runtimeError(msg('error-precondition-not-met', 'with-dash', '1', 'list of numbers', es), app))
    }
  }
  return Promise.resolve(ok(withDash(es as number[], args[1] as Drawing)))
}

const imageEntry = (prim: L.Prim, docs?: L.Doc) => L.entry(L.vprim(prim), 'image', undefined, docs)

export const imageLib: L.Env = new L.Env([
  ['image?', imageEntry(imagePrim, Docs.image)],
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
  ['with-dashes', imageEntry(withDashPrim, Docs.withDashes)]
])
