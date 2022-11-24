import { msg } from '../messages.js'
import { ok } from '../result.js'
import { runtimeError } from '../runtime.js'
import * as E from '../evaluator.js'
import * as L from '../lang.js'
import * as Utils from './utils.js'

const withCanvasDoc: L.Doc = new L.Doc(
  '(with-canvas width height renderer) -> canvas?', [
    'width: integer?, non-negative',
    'height: integer?, non-negative',
    'renderer: procedure?'
  ],
  'Renders a canvas of the given dimensions to the screen, using the renderer function to draw its contents. Renderer takes a context as input and calls various rendering functions on that context.'
)

const withCanvasPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('with-canvas', ['integer?', 'integer?', 'procedure?'], undefined, args, app).andThen(_ =>
    ok({
      renderAs: 'canvas',
      width: args[0],
      height: args[1],
      renderer: args[2]
    })
  ))

const rectangleDoc: L.Doc = new L.Doc(
  '(rectangle ctx x y width height) -> void?', [
    'ctx: context?',
    'x: integer?',
    'y: integer?',
    'width: integer?, non-negative',
    'height: integer?, non-negative',
    'mode: string?, either `"solid"` or `"outline"`',
    'color: string?'
  ],
  'Renders a rectangle whose upper-left corner is at `(x, y)`.'
)

const rectanglePrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('rectangle', ['any', 'integer?', 'integer?', 'integer?', 'integer?', 'string?', 'string?'], undefined, args, app).andThen(_ => {
    // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
    const ctx: CanvasRenderingContext2D = args[0] as any
    const mode = args[5] as string
    const color = args[6] as string
    ctx.fillStyle = color
    ctx.strokeStyle = color
    if (mode === 'solid') {
      ctx.fillRect(
        args[1] as number,
        args[2] as number,
        args[3] as number,
        args[4] as number
      )
    } else if (mode === 'outline') {
      ctx.strokeRect(
        args[1] as number,
        args[2] as number,
        args[3] as number,
        args[4] as number
      )
    } else {
      return runtimeError(msg('error-precondition-not-met', 'rectangle', '6', '"solid" or "outline"', mode), app)
    }
    return ok(undefined)
  }))

const ellipseDoc = new L.Doc(
  '(ellipse ctx x y radiusX radiusY rotation startAngle endAngle mode color) -> void?', [
    'ctx: context?',
    'x: number?',
    'y: number?',
    'radiusX: number?, non-negative',
    'radiusY: number?, non-negative',
    'rotation: number?',
    'startAngle: number?',
    'endAngle: number?',
    'mode: string?, either `"solid"` or `"outline"`',
    'color: string?'
  ],
  'Renders an ellipse whose center is at `(x, y)`, radii `radiusX` and `radiusY`, `rotation`, and `startAngle`, and `endAngle`.'
)

const ellipsePrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('ellipse', ['any', 'number?', 'number?', 'number?', 'number?', 'number?', 'number?', 'number?', 'string?', 'string?'], undefined, args, app).andThen(_ => {
    // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
    const ctx: CanvasRenderingContext2D = args[0] as any
    const mode = args[8] as string
    const color = args[9] as string
    ctx.fillStyle = color
    ctx.strokeStyle = color
    ctx.beginPath()
    ctx.ellipse(
      args[1] as number, // x
      args[2] as number, // y
      args[3] as number, // radiusX
      args[4] as number, // radiusY
      args[5] as number, // rotation
      args[6] as number, // startAngle
      args[7] as number // endAngle
    )
    if (mode === 'solid') {
      ctx.fill()
    } else if (mode === 'outline') {
      ctx.stroke()
    } else {
      return runtimeError(msg('error-precondition-not-met', 'ellipse', '9', '"solid" or "outline"', mode), app)
    }
    return ok(undefined)
  }))

const textDoc: L.Doc = new L.Doc(
  '(text ctx text x y mode color font) -> void?', [
    'ctx: context?',
    'text: string?',
    'x: integer?',
    'y: integer?',
    'mode: string?, either `"solid"` or `"outline"`',
    'color: string?',
    'font: string", a css font string, e.g., `"24px sans-serif"`'
  ],
  'Renders the given text at the given coordinates.'
)

const textPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('text', ['any', 'string?', 'integer?', 'integer?', 'string?', 'string?', 'string?'], undefined, args, app).andThen(_ => {
    // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
    const ctx: CanvasRenderingContext2D = args[0] as any
    const mode = args[4] as string
    const color = args[5] as string
    const font = args[6] as string
    ctx.fillStyle = color
    ctx.strokeStyle = color
    ctx.font = font
    if (mode === 'solid') {
      ctx.fillText(args[1] as string, args[2] as number, args[3] as number)
    } else if (mode === 'outline') {
      ctx.strokeText(args[1] as string, args[2] as number, args[3] as number)
    } else {
      return runtimeError(msg('error-precondition-not-met', 'text', '5', '"solid" or "outline"', mode), app)
    }
    return ok(undefined)
  }))

const animateWithDoc: L.Doc = new L.Doc(
  '(animate-with renderer) -> void?', [
    'renderer: procedure?, takes a single argument, the current time in milliseconds.'
  ],
  'Calls renderer repeatedly, allowing for animation-style behavior,'
)

const animateWithPrim: L.Prim = (env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('animate-with', ['procedure?'], undefined, args, app).andThen(_ => {
    async function callback (time: number) {
      await E.evaluateExp(env, L.nlecall(L.nlevalue(args[0]), [L.nlevalue(time)]))
      // eslint-disable-next-line @typescript-eslint/no-misused-promises
      window.requestAnimationFrame(callback)
    }
    // eslint-disable-next-line @typescript-eslint/no-misused-promises
    window.requestAnimationFrame(callback)
    return ok(undefined)
  }))

const canvasEntry = (prim: L.Prim, docs?: L.Doc) => L.entry(L.vprim(prim), 'canvas', undefined, docs)

export const canvasLib: L.Env = new L.Env([
  ['with-canvas', canvasEntry(withCanvasPrim, withCanvasDoc)],
  ['rectangle', canvasEntry(rectanglePrim, rectangleDoc)],
  ['ellipse', canvasEntry(ellipsePrim, ellipseDoc)],
  ['text', canvasEntry(textPrim, textDoc)],
  ['animate-with', canvasEntry(animateWithPrim, animateWithDoc)]
])
