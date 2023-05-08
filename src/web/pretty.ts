/* eslint-disable @typescript-eslint/no-unsafe-member-access */
/* eslint-disable @typescript-eslint/no-unsafe-assignment */
import * as L from '../lang.js'

import * as Audio from './audio.js'
import * as Files from './files.js'
import * as Image from './image.js'
import * as Music from './music.js'

type Renderer = (obj: any) => HTMLElement
export const renderers = new Map<string, Renderer>([
  ['audio', Audio.audioRenderer],
  ['audio-pipeline', Audio.audioPipelineRenderer],
  ['composition', Music.renderer],
  ['drawing', Image.renderer],
  ['reactive-file', Files.renderer]
])

export function makeCodeElt (text: string): HTMLElement {
  const ret = document.createElement('code')
  ret.appendChild(document.createTextNode(text))
  return ret
}

export function valueToNode (v: L.Value): HTMLElement {
  if (typeof v === 'boolean') {
    return makeCodeElt(v ? '#t' : '#f')
  } else if (typeof v === 'number') {
    return makeCodeElt(`${v.toString()}`)
  } else if (typeof v === 'string') {
    return makeCodeElt(`"${v}"`)
  } else if (v === null) {
    return makeCodeElt('null')
  } else if (v === undefined) {
    return makeCodeElt('void')
  } else if (Array.isArray(v)) {
    const arr = v as L.Value[]
    if (arr.length === 0) {
      return makeCodeElt('(vector)')
    } else {
      const ret = makeCodeElt('(vector ')
      ret.appendChild(valueToNode(arr[0]))
      arr.slice(1).forEach((v) => {
        ret.appendChild(document.createTextNode(' '))
        ret.appendChild(valueToNode(v))
      })
      ret.appendChild(document.createTextNode(')'))
      return ret
    }
  } else if (L.valueIsLambda(v)) {
    return makeCodeElt('[object Function]')
  } else if (L.valueIsPrim(v)) {
    return makeCodeElt('[object Function]')
  } else if (L.valueIsChar(v)) {
    const ch = (v as L.CharType).value
    let printed = ch
    switch (ch) {
      // TODO: probably add in special cases for... all the other cases!
      case ' ': printed = 'space'; break
      default: break
    }
    return makeCodeElt(`#\\${printed}`)
  } else if (L.valueIsPair(v)) {
    if (L.valueIsList(v)) {
      const arr = L.valueListToArray_(v)
      const ret = makeCodeElt('(list ')
      ret.appendChild(valueToNode(arr[0]))
      arr.slice(1).forEach((v) => {
        ret.appendChild(document.createTextNode(' '))
        ret.appendChild(valueToNode(v))
      })
      ret.appendChild(document.createTextNode(')'))
      console.log(ret)
      return ret
    } else {
      const p = v as L.PairType
      const ret = makeCodeElt('(pair ')
      ret.appendChild(valueToNode(p.fst))
      ret.appendChild(document.createTextNode(' '))
      ret.appendChild(valueToNode(p.snd))
      ret.appendChild(document.createTextNode(')'))
      return ret
    }
  } else if (L.valueIsStruct(v)) {
    const s = v as L.StructType
    if (s.fields.length === 0) {
      return makeCodeElt(`(struct ${s.kind.toString()} ())`)
    } else {
      const ret = makeCodeElt(`(struct ${s.kind.toString()} `)
      ret.appendChild(valueToNode(s.fields[0]))
      s.fields.slice(1).forEach((v) => {
        ret.appendChild(document.createTextNode(' '))
        ret.appendChild(valueToNode(v))
      })
      ret.appendChild(document.createTextNode(')'))
      return ret
    }
  } else if (v instanceof HTMLElement) {
    return v
  } else {
    if (Object.hasOwn(v, 'renderAs')) {
      const tag: string = (v as any).renderAs
      if (renderers.has(tag)) {
        return renderers.get(tag)!(v)
      }
    }
    return makeCodeElt('[object Object]')
  }
}
