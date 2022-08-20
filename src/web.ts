import { Exp, isList, Lit, Program, Stmt, stmtToString, unsafeListToArray } from './lang.js'
import { detailsToCompleteString } from './result.js'

export const outputClass = 'scamper-output'

export function emitSupportScript (): string {
  // N.B.: this is a hack to avoid importing modules cross-package into webpages
  // that manipulate and view Scamper code. Ideally, we could either reliably
  // solve the package import problem (in particular, in a VSCode webview
  // embedded in a web extension) or reliably take a Javascript function (or
  // transpiled Typescript function) and extract its source code.
  return `<script>

  // Drawing

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

  function clearDrawing (canvas) {
    const ctx = canvas.getContext('2d')
    ctx.fillStyle = 'white'
    ctx.strokeStyle = 'black'
    ctx.fillRect(0, 0, canvas.width, canvas.height)
  }
  
  function renderDrawing (x, y, drawing, canvas) {
    clearDrawing(canvas)
    render(x, y, drawing.width, drawing.height, drawing, canvas)
  }

  function emitDrawingWidget(node) {
    const canvas = document.createElement('canvas')
    const drawing = JSON.parse(node.innerText)
    canvas.width = drawing.width
    canvas.height = drawing.height
    renderDrawing(0, 0, drawing, canvas)
    node.replaceWith(canvas)
  }

  // Composition

  function ratioToDouble(ratio) {
    return ratio.num / ratio.den
  }

  function durationToTimeMs(beat, bpm, dur) {
    return ratioToDouble(dur) / (ratioToDouble(beat) * bpm) * 60 * 1000
  }

  function compositionToMsgs(beat, bpm, startTime, composition) {
    switch (composition.tag) {
      case 'note':
        const endTime = startTime + durationToTimeMs(beat, bpm, composition.duration)
        return {
          endTime: endTime,
          msgs: [
            {
              time: startTime,
              data: JZZ.MIDI.noteOn(0, composition.pitch + composition.octave, 127)
            },
            {
              time: endTime,
              data: JZZ.MIDI.noteOff(0, composition.pitch + composition.octave, 127)
            }
          ]
        }

      case 'rest':
        return {
          endTime: startTime + durationToTimeMs(beat, bpm, composition.duration),
          msgs: []
        }

      case 'par': {
        const msgs = []
        let endTime = 0
        composition.notes.forEach(note => {
          const result = compositionToMsgs(beat, bpm, startTime, note)
          msgs.push(...result.msgs)
          endTime = Math.max(result.endTime, endTime)
        })
        msgs.sort((c1, c2) => c1.time - c2.time)
        return { endTime: endTime, msgs }
      }

      case 'seq': {
        const msgs = []
        let time = startTime
        composition.notes.forEach(note => {
          const result = compositionToMsgs(beat, bpm, time, note)
          msgs.push(...result.msgs)
          time = result.endTime
          console.log(time)
        })
        msgs.sort((c1, c2) => c1.time - c2.time)
        return { endTime: time, msgs }
      }

      case 'mod':
        // TODO: fill in once we have mods!
        return compositionToMsgs(note)
    }
  }

  function playback(synth, composition) {
    const startTime = window.performance.now()
    console.log(composition)
    const msgs = compositionToMsgs({num: 1, den: 4}, 120, 0, composition).msgs
    console.log(msgs)
    let i = 0
    const id = setInterval(() => {
      const now = window.performance.now()
      while (i < msgs.length) {
        if (msgs[i].time + startTime <= now) {
          synth.send(msgs[i].data)
          i += 1
        } else {
          return
        }
      }
      clearInterval(id)
    })
    return id
  }

  function emitCompositionWidget(node) {
    const synth = JZZ.synth.Tiny()
    const composition = JSON.parse(node.textContent)
    node.textContent = ''  // N.B., clear the contents of the node for the buttons
    const playButton = document.createElement('button')
    playButton.textContent = '▶'
    const stopButton = document.createElement('button')
    stopButton.textContent = '■'
    let timer = undefined
    playButton.onclick = function(_e) {
      timer = playback(synth, composition)
    }
    stopButton.onclick = function(_e) {
      if (timer !== undefined) {
        clearInterval(timer)
      }
    }
    node.appendChild(playButton)
    node.appendChild(stopButton)
    console.log('Composition widget made')
    console.log(node.textContent)
  }

  function emitWidgets() {
    Array.from(document.getElementsByClassName('drawing')).forEach(emitDrawingWidget)
    Array.from(document.getElementsByClassName('composition')).forEach(emitCompositionWidget)
  }

</script>`
}

function outputDiv (i: number, s: string): string {
  return `<div class="${outputClass}" id="${outputClass}-${i}">${s}</div>`
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
      if (e.kind === 'Drawing') {
        return `<span class="drawing">${JSON.stringify(e.obj)}</span>`
      } else if (e.kind === 'Composition') {
        return `<span class="composition">${JSON.stringify(e.obj)}</span>`
      } else {
        return `[object ${e.kind}]`
      }
    case 'prim': return `[prim ${e.prim.name}]`
  }
}

export function stmtToHtml (s: Stmt, outputBinders: boolean = false): string {
  switch (s.tag) {
    case 'error':
      return s.errors.map(detailsToCompleteString).join('<br/>')
    case 'value':
      return expToHtml(s.value)
    case 'binding':
      return outputBinders ? stmtToString(s, true) : ''
    case 'imported':
      return outputBinders ? stmtToString(s, true) : ''
    case 'import':
      return stmtToString(s)
    case 'define':
      return `(define ${s.name.value} ${expToHtml(s.value)})`
    case 'exp':
      return expToHtml(s.value)
  }
}

export function programToHtml (program: Program, outputBinders: boolean = false): string {
  return program.statements.map((s, i) => outputDiv(i, stmtToHtml(s, outputBinders))).join('\n')
}
