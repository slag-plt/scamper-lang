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
    case 'beside': {
      drawing.drawings.forEach(d => {
        render(x, y + (drawing.height - d.height) / 2, d, canvas)
        x += d.width
      })
      break
    }
    case 'above': {
      drawing.drawings.forEach(d => {
        render(x + (drawing.width - d.width) / 2, y, d, canvas)
        y += d.height
      })
      break
    }
    case 'overlay': {
      // N.B., need to draw in reverse order to get the overlay effect to work
      [...drawing.drawings].reverse().forEach(d => {
        render(x + (drawing.width - d.width) / 2, y + (drawing.height - d.height) / 2, d, canvas)
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
  render(x, y, drawing, canvas)
}

export function emitDrawingWidget(node: Element) {
  const canvas = document.createElement('canvas')
  const drawing = JSON.parse(node.textContent!)
  canvas.width = drawing.width
  canvas.height = drawing.height
  console.log('==========')
  renderDrawing(0, 0, drawing, canvas)
  node.replaceWith(canvas)
}