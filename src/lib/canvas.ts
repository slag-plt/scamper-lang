import { msg } from '../messages.js'
import { errorToString, ok } from '../result.js'
import { runtimeError } from '../runtime.js'
import * as E from '../evaluator.js'
import * as L from '../lang.js'
import * as Utils from './utils.js'

const canvasDoc: L.Doc = new L.Doc(
  '(canvas width height) -> canvas?', [
    'width: integer?, positive',
    'height: integer?, positive'
  ],
  'Creates a canvas with the given width and height.'
)

const canvasPrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('canvas', ['integer?', 'integer?'], undefined, args, app).andThen(_ => {
    const canvas = document.createElement('canvas')
    canvas.width = args[0] as number
    canvas.height = args[1] as number
    return ok(canvas)
  }))

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
    const ctx: CanvasRenderingContext2D = (args[0] as HTMLCanvasElement).getContext('2d')!
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
  '(ellipse canvas x y radiusX radiusY rotation startAngle endAngle mode color) -> void?', [
    'canvas: canvas?',
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
    const ctx: CanvasRenderingContext2D = (args[0] as HTMLCanvasElement).getContext('2d')!
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

const circleDoc = new L.Doc(
  '(circle canvas x y radius mode color) -> void?', [
    'canvas: canvas?',
    'x: number?',
    'y: number?',
    'radius: number?, non-negative',
    'mode: string?, either `"solid"` or `"outline"`',
    'color: string?'
  ],
  'Renders a circle whose center is at `(x, y)` and radius `radius`.'
)

const circlePrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('circle', ['any', 'number?', 'number?', 'number?', 'string?', 'string?'], undefined, args, app).andThen(_ => {
    // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
    const ctx: CanvasRenderingContext2D = (args[0] as HTMLCanvasElement).getContext('2d')!
    const mode = args[4] as string
    const color = args[5] as string
    ctx.fillStyle = color
    ctx.strokeStyle = color
    ctx.beginPath()
    ctx.arc(
      args[1] as number, // x
      args[2] as number, // y
      args[3] as number, // radius
      0, // startAngle
      2 * Math.PI // endAngle
    )
    if (mode === 'solid') {
      ctx.fill()
    } else if (mode === 'outline') {
      ctx.stroke()
    } else {
      return runtimeError(msg('error-precondition-not-met', 'circle', '5', '"solid" or "outline"', mode), app)
    }
    return ok(undefined)
  }))

const textDoc: L.Doc = new L.Doc(
  '(text canvas text x y mode color font) -> void?', [
    'canvas: canvas?',
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
    const ctx: CanvasRenderingContext2D = (args[0] as HTMLCanvasElement).getContext('2d')!
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

const imageDoc: L.Doc = new L.Doc(
  '(image path) -> void?', [
    'path: string?, the path to the image'
  ],
  'Constructs an image value from the given path.'
)

const imagePrim: L.Prim = (_env, args, app) =>
  Utils.checkArgsResult('image', ['string?'], undefined, args, app).asyncAndThen(async _ => {
    const image = new Image()
    await new Promise(resolve => {
      console.log('...')
      image.onload = resolve
      image.src = args[0] as string
    })
    console.log(`image.src = ${image.src}`)
    return ok(image)
  })

const drawImageDoc: L.Doc = new L.Doc(
  '(draw-image canvas image x y) -> void?', [
    'canvas: canvas?',
    'image: image?',
    'x: integer?',
    'y: integer?'
  ],
  'Draws the given image at the given coordinates.'
)

const drawImagePrim: L.Prim = (_env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('draw-image', ['any', 'any', 'number?', 'number?'], undefined, args, app).andThen(_ => {
    // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
    const ctx: CanvasRenderingContext2D = (args[0] as HTMLCanvasElement).getContext('2d')!
    // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
    const image: HTMLImageElement = args[1] as any
    ctx.drawImage(image, args[2] as number, args[3] as number)
    return ok(undefined)
  }))

const pathDoc: L.Doc = new L.Doc(
  '(path canvas pairs mode color) -> void?', [
    'canvas: canvas?',
    'pairs: list?, a list of pairs of numbers',
    'mode: string?, either `"solid"` or `"outline"`',
    'color: string?'
  ],
  'Renders a path from the given list of pairs of numbers.'
)

const pathPrim: L.Prim = (_env, args, app) => {
  const argErr = Utils.checkArgs('path', ['any', 'list?', 'string?', 'string?'], undefined, args, app)
  if (argErr) { return Promise.resolve(argErr) }
  const ctx: CanvasRenderingContext2D = (args[0] as HTMLCanvasElement).getContext('2d')!
  const pairs = args[1] as [number, number][]
  const mode = args[2] as string
  const color = args[3] as string
  if (mode !== 'solid' && mode !== 'outline') {
    return Promise.resolve(runtimeError(msg('error-precondition-not-met', 'path', '3', '"solid" or "outline"', mode), app))
  }
  if (pairs.length === 0) {
    return Promise.resolve(ok(undefined))
  }
  ctx.fillStyle = color
  ctx.strokeStyle = color
  ctx.beginPath()
  ctx.moveTo(pairs[0][0], pairs[0][1])
  for (let i = 1; i < pairs.length; i++) {
    ctx.lineTo(pairs[i][0], pairs[i][1])
  }
  if (mode === 'solid') {
    ctx.fill()
  } else {
    ctx.stroke()
  }
  return Promise.resolve(ok(undefined))
}

const animateWithDoc: L.Doc = new L.Doc(
  '(animate-with proc) -> void?', [
    'proc: procedure?, a procedure that takes the current time in milliseconds as input.'
  ],
  'Repeatedly calls `proc` approximately once every 60 seconds, creating the effect of animation.'
)

const animateWithPrim: L.Prim = (env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('animate-with', ['procedure?'], undefined, args, app).andThen(_ => {
    async function callback (time: number) {
      const result = await E.evaluateExp(env, L.nlecall(L.nlevalue(args[0]), [L.nlevalue(time)]))
      if (result.tag !== 'error') {
        // eslint-disable-next-line @typescript-eslint/no-misused-promises
        window.requestAnimationFrame(callback)
      } else {
        console.log(errorToString(result))
      }
    }
    // eslint-disable-next-line @typescript-eslint/no-misused-promises
    window.requestAnimationFrame(callback)
    return ok(undefined)
  }))

const canvasOnclickDoc: L.Doc = new L.Doc(
  '(canvas-onclick canvas proc) -> void?', [
    'canvas: canvas?',
    'proc: procedure?, a procedure that takes two arguments: numbers representing the x and y coordinate of the mouse click on the canvas.'
  ],
  'Sets the given procedure to be called when the canvas is clicked by the user.'
)

const canvasOnclickPrim: L.Prim = (env, args, app) =>
  Promise.resolve(Utils.checkArgsResult('canvas-onclick', ['any', 'procedure?'], undefined, args, app).andThen(_ => {
    const canvas = args[0] as HTMLCanvasElement
    const fn = args[1] as L.FunctionType
    canvas.onclick = async function (ev: MouseEvent) {
      await E.evaluateExp(env, L.nlecall(L.nlevalue(fn), [L.nlevalue(ev.x), L.nlevalue(ev.y)]))
    }
    return ok(undefined)
  }))

const canvasEntry = (prim: L.Prim, docs?: L.Doc) => L.entry(L.vprim(prim), 'canvas', undefined, docs)

export const canvasLib: L.Env = new L.Env([
  ['canvas', canvasEntry(canvasPrim, canvasDoc)],
  ['rectangle', canvasEntry(rectanglePrim, rectangleDoc)],
  ['ellipse', canvasEntry(ellipsePrim, ellipseDoc)],
  ['circle', canvasEntry(circlePrim, circleDoc)],
  ['text', canvasEntry(textPrim, textDoc)],
  ['image', canvasEntry(imagePrim, imageDoc)],
  ['draw-image', canvasEntry(drawImagePrim, drawImageDoc)],
  ['path', canvasEntry(pathPrim, pathDoc)],
  ['animate-with', canvasEntry(animateWithPrim, animateWithDoc)],
  ['canvas-onclick', canvasEntry(canvasOnclickPrim, canvasOnclickDoc)]
])
