import { Exp, isList, Lit, Program, Stmt, stmtToString, unsafeListToArray } from './lang.js'
import { errorDetailsToString } from './result'

export const outputClass = 'scamper-output'

export function emitSupportScript (): string {
  // N.B.: this is a hack to avoid importing modules cross-package into webpages
  // that manipulate and view Scamper code. Ideally, we could either reliably
  // solve the package import problem (in particular, in a VSCode webview
  // embedded in a web extension) or reliably take a Javascript function (or
  // transpiled Typescript function) and extract its source code.
  return `<script>
  function render (x, y, width, height, drawing, canvas) {
    const ctx = canvas.getContext('2d')
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
  
  function renderDrawing (x, y, drawing, canvas) {
    render(x, y, drawing.width, drawing.height, drawing, canvas)
  }
</script>`
}

let outputDivCounter = 0

function outputDiv (s: string): string {
  return `<div class="${outputClass}" id="${outputClass}-${outputDivCounter++}">${s}</div>`
}

function litToHtml (l: Lit): string {
  switch (l.tag) {
    case 'bool': return l.value ? '#t' : '#f'
    case 'num': return l.value.toString()
    case 'char': return `#${l.value}`
    case 'str': return l.value
  }
}

export function expToHtml (e: Exp): string {
  switch (e.tag) {
    case 'var': return e.value
    case 'lit': return litToHtml(e.value)
    case 'call': return `(${[e.head].concat(e.args).map(expToHtml).join(' ')})`
    case 'lam': return `(lambda (${e.args.map(n => n.value).join(' ')}) ${expToHtml(e.body)})`
    case 'if': return `(if ${expToHtml(e.e1)} ${expToHtml(e.e2)} ${expToHtml(e.e3)}`
    case 'nil': return 'null'
    case 'pair':
      return isList(e)
        ? `(list ${unsafeListToArray(e).map(expToHtml)})`
        : `(cons ${expToHtml(e.e1)} ${expToHtml(e.e2)})`
    case 'let': return `(let (${e.bindings.map(([x, e]) => `[${x} ${expToHtml(e)}]`).join(' ')}) ${expToHtml(e.body)})`
    case 'cond': return `(cond ${e.branches.map(b => `[${expToHtml(b[0])} ${expToHtml(b[1])}]`).join(' ')})`
    case 'and': return `(and ${e.args.map(expToHtml).join(' ')})`
    case 'or': return `(or ${e.args.map(expToHtml).join(' ')})`
    case 'obj':
      return e.kind !== 'Drawing'
        ? `[object ${e.kind}]`
        : `<canvas id="test"></canvas>
        <script>
          const canvas = document.getElementById("test")
          const drawing = ${JSON.stringify(e.obj)}
          canvas.width = drawing.width
          canvas.height = drawing.height
          renderDrawing(0, 0, drawing, canvas)
        </script>`
    case 'prim': return `[prim ${e.prim.name}]`
  }
}

export function stmtToHtml (s: Stmt, outputBinders: boolean = false): string {
  switch (s.tag) {
    case 'error':
      return outputDiv(s.errors.map(errorDetailsToString).join('<br/>'))
    case 'value':
      return outputDiv(expToHtml(s.value))
    case 'binding':
      return outputBinders ? outputDiv(stmtToString(s)) : ''
    case 'imported':
      return outputBinders ? outputDiv(stmtToString(s)) : ''
    case 'import':
      return outputDiv(stmtToString(s))
    case 'define':
      return outputDiv(stmtToString(s))
    case 'exp':
      return outputDiv(expToHtml(s.value))
  }
}

export function programToHtml (program: Program, outputBinders: boolean = false): string {
  return program.statements.map(s => stmtToHtml(s, outputBinders)).join('\n')
}
