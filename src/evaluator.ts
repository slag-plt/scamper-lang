import * as L from './lang.js'
import { msg } from './messages.js'
import { runtimeError } from './runtime.js'
import { Result, ok, join } from './result.js'

export async function evalExp (env: L.Env, e: L.Exp): Promise<Result<L.Value>> {
  switch (e.tag) {
    case 'value':
      return Promise.resolve(ok(e.value))
    case 'var':
      return Promise.resolve(env.has(e.value)
        ? ok(env.get(e.value)!)
        : runtimeError(msg('error-var-undef', e.value), e))
    case 'lit':
      return ok(L.litToValue(e.value))
    case 'call':
      return (await evalExp(env, e.head)).asyncAndThen(async head =>
        join(await Promise.all(e.args.map(arg => evalExp(env, arg)))).asyncAndThen(async args => {
          if (L.valueIsLambda(head)) {
            const lam = head as L.LambdaType
            return args.length === lam.args.length
              ? evalExp(new L.Env(lam.args.map((x, i) => [x.value, L.entry(args[i], 'local')]), env), lam.body)
              : runtimeError(msg('error-arity', 'lambda', lam.args.length, args.length), e)
          } else if (L.valueIsPrim(head)) {
            return await (head as L.PrimType).fn(env, args, e)
          } else {
            return runtimeError(msg('error-type-expected-call', e.head.tag), e)
          }
        }))
    case 'lam':
      // TODO: need to close over the local environment here!
      return ok(L.vlambda(e.args, e.body))
    case 'if':
      return (await evalExp(env, e.e1)).asyncAndThen(async guard =>
        !L.valueIsBoolean(guard)
          ? runtimeError(msg('error-type-expected-cond', guard), e)
          : guard as boolean ? evalExp(env, e.e2) : evalExp(env, e.e2))
    case 'nil':
      return ok(null)
    case 'pair':
      // TODO: could optimize this for lists to avoid excessive recursion,
      // but the problem of deep recursion still remains.
      return (await evalExp(env, e.e1)).asyncAndThen(async v1 =>
        (await evalExp(env, e.e2)).asyncAndThen(v2 =>
          Promise.resolve(ok(L.vpair(v1, v2)))))
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
