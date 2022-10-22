import * as L from './lang.js'
import { msg } from './messages.js'
import { runtimeError } from './runtime.js'
import { Result, ok } from './result.js'

export function evalExp (env: L.Env, e: L.Exp): Promise<Result<L.Value>> {
  switch (e.tag) {
    case 'value':
      return Promise.resolve(ok(e))
    case 'var':
      return Promise.resolve(env.has(e.value)
        ? ok(env.get(e.value)!)
        : runtimeError(msg('error-var-undef', e.value), e))
    case 'lit':
      return Promise.resolve(ok(e))
    case 'call':
      throw new Error('unimplemented!')
    case 'lam':
      return Promise.resolve(ok(e))
    case 'if':
      throw new Error('unimplemented!')
    case 'nil':
      throw new Error('unimplemented!')
    case 'pair':
      throw new Error('unimplemented!')
    case 'let':
      throw new Error('unimplemented!')
    case 'cond':
      throw new Error('unimplemented!')
    case 'and':
      throw new Error('unimplemented!')
    case 'or':
      throw new Error('unimplemented!')
    case 'match':
      throw new Error('unimplemented!')
  }
}
