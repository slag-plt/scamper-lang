import * as L from './lang'

function createSpan (cls?: string, content?: string): HTMLSpanElement {
  const e = document.createElement('span')
  e.classList.add('scamper-output')
  if (cls) {
    e.classList.add(cls)
  }
  if (content) {
    e.innerText = content
  }
  return e
}

export function renderValue (v: L.Value): HTMLElement {
  if (typeof v === 'boolean') {
    return createSpan('boolean', v ? '#t' : '#f')
  } else if (typeof v === 'number') {
    return createSpan('number', v.toString())
  } else if (typeof v === 'string') {
    return createSpan('string', `"${v}"`)
  } else if (L.valueIsChar(v)) {
    const ch = (v as L.CharType).value
    let printed = ch
    switch (ch) {
      // TODO: probably add in special cases for... all the other cases!
      case ' ': printed = 'space'; break
      default: break
    }
    return createSpan('char', `#\\${printed}`)
  } else if (L.valueIsLambda(v) || L.valueIsPrim(v)) {
    return createSpan(undefined, '[object Function]')
  } else {
    throw new Error('unimplemented!')
  }
}
