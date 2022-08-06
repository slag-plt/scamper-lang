import { ok } from '../result.js'
import { runtimeError } from '../interp.js'
import { asList_, asNum_, asString_, EObj, Exp, isInteger, isList, isString, nleobj } from '../lang.js'
import { msg } from '../messages.js'
import { Prim } from '../prims.js'

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

const circlePrim: Prim = (head, args, app) => {
  if (args.length !== 3) {
    return runtimeError(msg('error-arity', 'circle', 3, args.length), app)
  } else if (!isInteger(args[0])) {
    return runtimeError(msg('error-type-expected-fun', 'circle', 'integer', args[0].tag), app)
  } else if (!isString(args[1])) {
    return runtimeError(msg('error-type-expected-fun', 'circle', 'string', args[1].tag), app)
  } else if (!isString(args[2])) {
    return runtimeError(msg('error-type-expected-fun', 'circle', 'string', args[2].tag), app)
  } else {
    const radius = asNum_(args[0])
    const mode = asString_(args[1])
    const color = asString_(args[2])
    if (mode !== 'solid' && mode !== 'outline') {
      return runtimeError(msg('error-precondition-not-met', 'circle', '"solid" or "outline"', mode), app)
    } else {
      return ok(nleobj('Drawing', circle(radius, mode, color)))
    }
  }
}

type Rectangle = { tag: 'rectangle', width: number, height: number, mode: Mode, color: string }
const rectangle = (width: number, height: number, mode: Mode, color: string): Rectangle =>
  ({ tag: 'rectangle', width, height, mode, color })

const rectanglePrim: Prim = (head, args, app) => {
  if (args.length !== 4) {
    return runtimeError(msg('error-arity', 'rectangle', 4, args.length), app)
  } else if (!isInteger(args[0])) {
    return runtimeError(msg('error-type-expected-fun', 'rectangle', 'integer', args[0].tag), app)
  } else if (!isInteger(args[2])) {
    return runtimeError(msg('error-type-expected-fun', 'rectangle', 'integer', args[1].tag), app)
  } else if (!isString(args[2])) {
    return runtimeError(msg('error-type-expected-fun', 'rectangle', 'string', args[2].tag), app)
  } else if (!isString(args[3])) {
    return runtimeError(msg('error-type-expected-fun', 'rectangle', 'string', args[3].tag), app)
  } else {
    const width = asNum_(args[0])
    const height = asNum_(args[1])
    const mode = asString_(args[2])
    const color = asString_(args[3])
    if (mode !== 'solid' && mode !== 'outline') {
      return runtimeError(msg('error-precondition-not-met', 'rectangle', '"solid" or "outline"', mode), app)
    } else {
      return ok(nleobj('Drawing', rectangle(width, height, mode, color)))
    }
  }
}

type Beside = { tag: 'beside', width: number, height: number, drawings: Drawing[] }
const beside = (drawings: Drawing[]): Beside => ({
  tag: 'beside',
  width: drawings.reduce((acc, d) => acc + d.width, 0),
  height: drawings.reduce((acc, d) => Math.max(acc, d.height), 0),
  drawings
})

const besidePrim: Prim = (head, args, app) => {
  if (args.length !== 1) {
    return runtimeError(msg('error-arity', 'beside', 1, args.length), app)
  } else if (!isList(args[0])) {
    return runtimeError(msg('error-type-expected-fun', 'beside', 'list', args[0].tag), app)
  } else {
    const drawings = asList_(args[0])
    drawings.forEach(d => {
      if (!isDrawing(d)) {
        return runtimeError(msg('error-type-expected-fun', 'beside', 'Drawing', d.tag), app)
      }
    })
    return ok(nleobj('Drawing', beside(drawings.map(d => (d as EObj).obj as Drawing))))
  }
}

type Above = { tag: 'above', width: number, height: number, drawings: Drawing[] }
const above = (drawings: Drawing[]): Above => ({
  tag: 'above',
  width: drawings.reduce((acc, d) => Math.max(acc, d.width), 0),
  height: drawings.reduce((acc, d) => acc + d.height, 0),
  drawings
})

const abovePrim: Prim = (head, args, app) => {
  if (args.length !== 1) {
    return runtimeError(msg('error-arity', 'above', 1, args.length), app)
  } else if (!isList(args[0])) {
    return runtimeError(msg('error-type-expected-fun', 'above', 'list', args[0].tag), app)
  } else {
    const drawings = asList_(args[0])
    drawings.forEach(d => {
      if (!isDrawing(d)) {
        return runtimeError(msg('error-type-expected-fun', 'above', 'Drawing', d.tag), app)
      }
    })
    return ok(nleobj('Drawing', above(drawings.map(d => (d as EObj).obj as Drawing))))
  }
}

type Overlay = { tag: 'overlay', width: number, height: number, drawings: Drawing[] }
const overlay = (drawings: Drawing[]): Overlay => ({
  tag: 'overlay',
  width: drawings.reduce((acc, d) => Math.max(acc, d.width), 0),
  height: drawings.reduce((acc, d) => Math.max(acc, d.height), 0),
  drawings
})

const overlayPrim: Prim = (head, args, app) => {
  if (args.length !== 1) {
    return runtimeError(msg('error-arity', 'overlay', 1, args.length), app)
  } else if (!isList(args[0])) {
    return runtimeError(msg('error-type-expected-fun', 'overlay', 'list', args[0].tag), app)
  } else {
    const drawings = asList_(args[0])
    drawings.forEach(d => {
      if (!isDrawing(d)) {
        return runtimeError(msg('error-type-expected-fun', 'overlay', 'Drawing', d.tag), app)
      }
    })
    return ok(nleobj('Drawing', overlay(drawings.map(d => (d as EObj).obj as Drawing))))
  }
}

function render (x: number, y: number, width: number, height: number, drawing: Drawing, canvas: HTMLCanvasElement) {
  const ctx = canvas.getContext('2d')!
  switch (drawing.tag) {
    case 'circle': {
      ctx.fillStyle = drawing.color
      ctx.strokeStyle = drawing.color
      ctx.beginPath()
      ctx.arc(x + width / 2, y + height / 2, drawing.radius, 0, 2 * Math.PI)
      if (drawing.mode === 'solid') {
        ctx.fill()
      } else if (drawing.mode === 'outline') {
        ctx.stroke()
      }
      break
    }
    case 'rectangle': {
      ctx.fillStyle = drawing.color
      ctx.strokeStyle = drawing.color
      if (drawing.mode === 'solid') {
        ctx.fillRect(x + (width - drawing.width) / 2, y + (height - drawing.height) / 2, drawing.width, drawing.height)
      } else if (drawing.mode === 'outline') {
        ctx.strokeRect(x + (width - drawing.width) / 2, y + (height - drawing.height) / 2, drawing.width, drawing.height)
      }
      break
    }
    case 'beside': {
      drawing.drawings.forEach(d => {
        render(x, y, d.width, height, d, canvas)
        x += d.width
      })
      break
    }
    case 'above': {
      drawing.drawings.forEach(d => {
        render(x, y, width, d.height, d, canvas)
        y += d.height
      })
      break
    }
    case 'overlay': {
      // N.B., need to draw in reverse order to get the overlay effect to work
      [...drawing.drawings].reverse().forEach(d => {
        render(x, y, width, height, d, canvas)
      })
      break
    }
  }
}

function renderDrawing (x: number, y: number, drawing: Drawing, canvas: HTMLCanvasElement) {
  render(x, y, drawing.width, drawing.height, drawing, canvas)
}

const imageLib: Map<String, Prim> = new Map([
  ['circle', circlePrim],
  ['rectangle', rectanglePrim],
  ['beside', besidePrim],
  ['above', abovePrim],
  ['overlay', overlayPrim]
])

export {
  renderDrawing, imageLib
}
