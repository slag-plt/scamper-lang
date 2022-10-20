import * as L from '../lang.js'
import { msg } from '../messages.js'
import { ICE, Error, Result, ok } from '../result.js'
import { runtimeError } from '../runtime.js'

type ArgSpec = string

function specToPred (spec: ArgSpec): (arg: L.Value) => boolean {
  switch (spec) {
    case 'any':
      return () => true
    case 'number?':
      return L.isNumber
    case 'integer?':
      return L.isInteger
    case 'real?':
      return L.isReal
    case 'boolean?':
      return L.isBoolean
    case 'string?':
      return L.isString
    case 'char?':
      return L.isChar
    case 'procedure?':
      return L.isProcedure
    case 'pair?':
      return L.isPair
    case 'list?':
      return L.isList
    case 'prim?':
      return L.isPrim
    case 'struct?':
      return L.isStruct
    default:
      if (spec.length > 0 && /[A-Z]/.test(spec[0])) {
        return (e) => L.isStructKind(e, spec)
      } else {
        throw new ICE(
          'argSpecToPred',
          `Unrecognized spec: ${spec}`
        )
      }
  }
}

export function checkArgs (func: string, specs: ArgSpec[], restSpec: ArgSpec | undefined, args: L.Value[], call: L.Value): Error<any> | undefined {
  // First, check the arity of the call
  // N.B., these casts are safe because runtimeError returns an Error<T>.
  // Note that the typechecker can't infer this here, but can below!
  if (restSpec && args.length < specs.length) {
    return runtimeError(msg('error-arity-atleast', func, specs.length, args.length), call) as Error<any>
  } else if (!restSpec && args.length !== specs.length) {
    return runtimeError(msg('error-arity', func, specs.length, args.length), call) as Error<any>
  }
  // Now, check the types of the arguments.
  let i = 0
  for (const spec of specs) {
    if (!specToPred(spec)(args[i])) {
      return runtimeError(msg('error-type-expected-fun', func, spec, i + 1, args[i].tag), call) as Error<any>
    }
    i += 1
  }
  // Finally, check the types of the rest parameters if needed.
  if (restSpec) {
    for (; i < args.length; i++) {
      const arg = args[i]
      if (!specToPred(restSpec)(arg)) {
        return runtimeError(msg('error-type-expected-fun', func, restSpec, i + 1, arg.tag), call) as Error<any>
      }
    }
  }
  // If we get this far, then we're ok. There are no errors to return!
  return undefined
}

export function checkArgsResult (func: string, specs: ArgSpec[], restSpec: ArgSpec | undefined, args: L.Value[], call: L.Value): Result<null> {
  const err = checkArgs(func, specs, restSpec, args, call)
  return err || ok(null)
}
