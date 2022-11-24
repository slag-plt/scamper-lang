import { msg } from '../messages.js'
import { ok } from '../result.js'
import { runtimeError } from '../runtime.js'
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
  '(rectangle canvas x y width height) -> void?', [
    'canvas: canvas?',
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

const canvasEntry = (prim: L.Prim, docs?: L.Doc) => L.entry(L.vprim(prim), 'canvas', undefined, docs)

export const canvasLib: L.Env = new L.Env([
  ['with-canvas', canvasEntry(withCanvasPrim, withCanvasDoc)],
  ['rectangle', canvasEntry(rectanglePrim, rectangleDoc)]
])
