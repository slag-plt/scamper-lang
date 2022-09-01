import * as Image from '../lib/image.js'

type Drawing = Image.Drawing

function render (x: number, y: number, drawing: Drawing, canvas: HTMLCanvasElement) {
  const ctx = canvas.getContext('2d')!
  console.log(`render (${drawing.tag}): (${x}, ${y}), ${drawing.width}, ${drawing.height}`)
  switch (drawing.tag) {
    case 'ellipse': {
      ctx.fillStyle = drawing.color
      ctx.strokeStyle = drawing.color
      const radiusX = drawing.width / 2
      const radiusY = drawing.height / 2
      const centerX = x + radiusX
      const centerY = y + radiusY
      ctx.beginPath()
      ctx.ellipse(centerX, centerY, radiusX, radiusY, 0, 0, 2 * Math.PI)
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
        ctx.fillRect(x, y, drawing.width, drawing.height)
      } else if (drawing.mode === 'outline') {
        ctx.strokeRect(x, y, drawing.width, drawing.height)
      }
      break
    }
    case 'triangle': {
      ctx.fillStyle = drawing.color
      ctx.strokeStyle = drawing.color
      ctx.beginPath()
      // Start in the bottom-left corner of the triangle...
      ctx.moveTo(x, y + drawing.height)
      // Then go to the top corner...
      ctx.lineTo(x + drawing.width / 2, y)
      // And then the bottom-right corner...
      ctx.lineTo(x + drawing.width, y + drawing.height)
      // And back!
      ctx.lineTo(x, y + drawing.height)
      if (drawing.mode === 'solid') {
        ctx.fill()
      } else if (drawing.mode === 'outline') {
        ctx.stroke()
      }
      break
    }
    case 'path': {
      // TODO: path fills won't render. I wonder why...
      console.log('Drawing path...')
      ctx.beginPath()
      ctx.fillStyle = drawing.color
      ctx.strokeStyle = drawing.color
      drawing.points.forEach(p => {
        console.log(`(${p[0]}, ${p[1]})`)
        ctx.lineTo(x + p[0], y + p[1])
      })
      ctx.closePath()
      if (drawing.mode === 'solid') {
        ctx.fill('evenodd')
      } else {
        ctx.stroke()
      }
      break
    }
    case 'beside': {
      drawing.drawings.forEach(d => {
        render(
          x,
          drawing.align === 'top'
            ? y
            : drawing.align === 'bottom'
              ? y + drawing.height - d.height
              // N.B., assumed to be 'center'
              : y + (drawing.height - d.height) / 2,
          d,
          canvas)
        x += d.width
      })
      break
    }
    case 'above': {
      drawing.drawings.forEach(d => {
        render(
          drawing.align === 'left'
            ? x
            : drawing.align === 'right'
              ? x + drawing.width - d.width
              // N.B., assumed to be 'middle'
              : x + (drawing.width - d.width) / 2,
          y,
          d,
          canvas)
        y += d.height
      })
      break
    }
    case 'overlay': {
      // N.B., need to draw in reverse order to get the overlay effect to work
      [...drawing.drawings].reverse().forEach(d => {
        render(
          drawing.xAlign === 'left'
            ? x
            : drawing.xAlign === 'right'
              ? x + drawing.width - d.width
              // N.B., assumed to be 'middle'
              : x + (drawing.width - d.width) / 2,
          drawing.yAlign === 'top'
            ? y
            : drawing.yAlign === 'bottom'
              ? y + drawing.height - d.height
              // N.B., assumed to be 'center'
              : y + (drawing.height - d.height) / 2,
          d,
          canvas)
      })
      break
    }
    case 'overlayOffset': {
      const x1 = drawing.dx > 0 ? x : x + Math.abs(drawing.dx)
      const y1 = drawing.dy > 0 ? y : y + Math.abs(drawing.dy)
      const x2 = drawing.dx > 0 ? x + drawing.dx : x
      const y2 = drawing.dy > 0 ? y + drawing.dy : y
      // N.B., render d2 first so d1 is on top
      render(x2, y2, drawing.d2, canvas)
      render(x1, y1, drawing.d1, canvas)
      break
    }
    case 'rotate': {
      ctx.translate(x + drawing.width / 2, y + drawing.height / 2)
      ctx.rotate(drawing.angle * Math.PI / 180)
      ctx.translate(x - drawing.width / 2, y - drawing.height / 2)
      render(x, y, drawing.drawing, canvas)
      ctx.resetTransform()
    }
  }
}

function clearDrawing (canvas: HTMLCanvasElement) {
  const ctx = canvas.getContext('2d')!
  ctx.fillStyle = 'white'
  ctx.strokeStyle = 'black'
  ctx.fillRect(0, 0, canvas.width, canvas.height)
}

function renderDrawing (x: number, y: number, drawing: Drawing, canvas: HTMLCanvasElement) {
  clearDrawing(canvas)
  render(x, y, drawing, canvas)
}

export function emitDrawingWidget(node: Element) {
  const canvas = document.createElement('canvas')
  const drawing = JSON.parse(node.textContent!)
  canvas.width = drawing.width
  canvas.height = drawing.height
  renderDrawing(0, 0, drawing, canvas)
  node.replaceWith(canvas)
}