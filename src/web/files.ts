/* eslint-disable @typescript-eslint/no-unsafe-member-access */
import * as Scamper from '../index.js'
import * as Pretty from './pretty.js'

export function renderer (obj: any): HTMLElement {
  const callback = obj.callback as Scamper.Lang.FunctionType
  const ret = document.createElement('code')
  const inp = document.createElement('input')
  const outp = document.createElement('code')
  inp.type = 'file'
  inp.addEventListener('change', () => {
    const reader = new FileReader()
    reader.onload = async (e) => {
      if (e !== null && e.target !== null) {
        const v = await Scamper.evaluateExp(Scamper.preludeEnv, Scamper.Lang.nlecall(Scamper.Lang.nlevalue(callback), [Scamper.Lang.nlevalue(e.target.result)]))
        if (v.tag === 'error') {
          outp.innerHTML = Scamper.errorToString(v)
        } else {
          outp.innerHTML = ''
          outp.appendChild(Pretty.valueToNode(v.value))
        }
      } else {
        outp.innerText = ''
      }
    }
    if (inp.files !== null && inp.files.length > 0) {
      outp.innerText = 'Loading...'
      reader.readAsText(inp.files[0])
    }
  }, false)

  ret.appendChild(inp)
  ret.appendChild(document.createElement('br'))
  ret.appendChild(outp)
  return ret
}
