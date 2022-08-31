import * as Image from '../lib/image.js'

type Drawing = Image.Drawing

function render (x: number, y: number, width: number, height: number, drawing: Drawing, canvas: HTMLCanvasElement) {
  const ctx = canvas.getContext('2d')!
  switch (drawing.tag) {
    case 'ellipse': {
      ctx.fillStyle = drawing.color
      ctx.strokeStyle = drawing.color
      const radiusX = drawing.width / 2
      const radiusY = drawing.height / 2
      const centerX = x + radiusX + (width - drawing.width) / 2
      const centerY = y + radiusY + (height - drawing.height) / 2
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
        ctx.fillRect(x + (width - drawing.width) / 2, y + (height - drawing.height) / 2, drawing.width, drawing.height)
      } else if (drawing.mode === 'outline') {
        ctx.strokeRect(x + (width - drawing.width) / 2, y + (height - drawing.height) / 2, drawing.width, drawing.height)
      }
      break
    }
    case 'triangle': {
      ctx.fillStyle = drawing.color
      ctx.strokeStyle = drawing.color
      ctx.beginPath()
      // Start in the bottom-left corner of the triangle...
      ctx.moveTo(x, y + height)
      // Then go to the top corner...
      ctx.lineTo(x + width / 2, y)
      // And then the bottom-right corner...
      ctx.lineTo(x + width, y + height)
      // And back!
      ctx.lineTo(x, y + height)
      if (drawing.mode === 'solid') {
        ctx.fill()
      } else if (drawing.mode === 'outline') {
        ctx.stroke()
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

function clearDrawing (canvas: HTMLCanvasElement) {
  const ctx = canvas.getContext('2d')!
  ctx.fillStyle = 'white'
  ctx.strokeStyle = 'black'
  ctx.fillRect(0, 0, canvas.width, canvas.height)
}

function renderDrawing (x: number, y: number, drawing: Drawing, canvas: HTMLCanvasElement) {
  clearDrawing(canvas)
  render(x, y, drawing.width, drawing.height, drawing, canvas)
}

export function emitDrawingWidget(node: Element) {
  const canvas = document.createElement('canvas')
  const drawing = JSON.parse(node.textContent!)
  canvas.width = drawing.width
  canvas.height = drawing.height
  renderDrawing(0, 0, drawing, canvas)
  node.replaceWith(canvas)
}