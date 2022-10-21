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
      return L.valueIsNumber
    case 'integer?':
      return L.valueIsInteger
    case 'real?':
      return L.valueIsReal
    case 'boolean?':
      return L.valueIsBoolean
    case 'string?':
      return L.valueIsString
    case 'char?':
      return L.valueIsChar
    case 'procedure?':
      return L.valueIsProcedure
    case 'pair?':
      return L.valueIsPair
    case 'list?':
      return L.valueIsList
    case 'prim?':
      return L.valueIsPrim
    case 'struct?':
      return L.valueIsStruct
    case 'drawing':
    case 'composition':
      // HACK: to quickly enable checking for our two special objects, drawing and composition,
      // which are raw JS objects rather than structs.
      // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
      return e => L.valueHasPropertyValue(e, 'renderAs', spec)
    default:
      if (spec.length > 0 && /[A-Z]/.test(spec[0])) {
        return e => L.valueIsStructKind(e, spec)
      } else {
        throw new ICE(
          'argSpecToPred',
          `Unrecognized spec: ${spec}`
        )
      }
  }
}

export function checkArgs (func: string, specs: ArgSpec[], restSpec: ArgSpec | undefined, args: L.Value[], call: L.Exp): Error<any> | undefined {
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
      return runtimeError(msg('error-type-expected-fun', func, spec, i + 1, args[i]), call) as Error<any>
    }
    i += 1
  }
  // Finally, check the types of the rest parameters if needed.
  if (restSpec) {
    for (; i < args.length; i++) {
      const arg = args[i]
      if (!specToPred(restSpec)(arg)) {
        return runtimeError(msg('error-type-expected-fun', func, restSpec, i + 1, arg), call) as Error<any>
      }
    }
  }
  // If we get this far, then we're ok. There are no errors to return!
  return undefined
}

export function checkArgsResult (func: string, specs: ArgSpec[], restSpec: ArgSpec | undefined, args: L.Value[], call: L.Exp): Result<null> {
  const err = checkArgs(func, specs, restSpec, args, call)
  return err || ok(null)
}
