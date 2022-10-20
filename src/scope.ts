import { ErrorDetails, errorDetails, ICE } from './result.js'
import { Env, Exp, Program, name, Name, fvarsOfPat } from './lang.js'
import { Range } from './loc.js'
import { msg } from './messages.js'
import { internalLibs, preludeEnv } from './runtime.js'
import { expToString } from './pretty.js'

function undefinedVariableError (x: string, range: Range): ErrorDetails {
  return errorDetails(
    msg('phase-scope'),
    msg('error-var-undef', x),
    range,
    x
  )
}

function repeatedVariableError (e: Exp, x: string, range: Range): ErrorDetails {
  return errorDetails(
    msg('phase-scope'),
    msg('error-var-repeated', x),
    range,
    expToString(0, e)
  )
}

function containsDups (xs: string[]): string | undefined {
  for (let i = 0; i < xs.length; i++) {
    for (let j = i + 1; j < xs.length; j++) {
      if (xs[i] === xs[j]) {
        return xs[i]
      }
    }
  }
  return undefined
}

function checkExp (bvars: string[], e: Exp): ErrorDetails[] {
  switch (e.tag) {
    case 'value':
      return []
    case 'var':
      return bvars.includes(e.value) ? [] : [undefinedVariableError(e.value, e.range)]
    case 'lit':
      return []
    case 'call':
      return checkExp(bvars, e.head).concat(e.args.flatMap((v) => checkExp(bvars, v)))
    case 'lam':
      return checkExp(bvars.concat(e.args.map(n => n.value)), e.body)
      // return e.args.filter(x => bvars.includes(x.value))
      //   .map(x => shadowedVariableError(x.value, x.range))
      //   .concat(checkExp(bvars.concat(e.args.map(n => n.value)), e.body))
    case 'if':
      return checkExp(bvars, e.e1).concat(checkExp(bvars, e.e2).concat(checkExp(bvars, e.e3)))
    case 'nil':
      return []
    case 'pair':
      return checkExp(bvars, e.e1).concat(checkExp(bvars, e.e2))
    case 'let': {
      let errors: ErrorDetails[] = []
      if (e.kind === 'let') {
        e.bindings.forEach(([_x, body]) => {
          errors = errors.concat(checkExp(bvars, body))
        })
        return errors.concat(
          checkExp(bvars.concat(
            e.bindings.map(([x, _body]) => x.value)), e.body))
      } else if (e.kind === 'let*') {
        e.bindings.forEach(binding => {
          bvars = bvars.concat(binding[0].value)
          errors = errors.concat(checkExp(bvars, binding[1]))
        })
        return errors.concat(checkExp(bvars, e.body))
      } else {
        // TODO: when we implement letrec, bindings can refer to earlier
        // ones, so we'll need to refine this check.
        throw new ICE('checkExp', 'letrec unimplemented')
      }
    }
    case 'cond':
      return e.branches.flatMap(b =>
        checkExp(bvars, b[0]).concat(checkExp(bvars, b[1])))
    case 'and':
      return e.args.flatMap((v) => checkExp(bvars, v))
    case 'or':
      return e.args.flatMap((v) => checkExp(bvars, v))
    // N.B., structs are internal, runtime only values, so we'll never
    // encounter these cases with our scope-checker.
    case 'match': {
      let errors = checkExp(bvars, e.scrutinee)
      e.branches.forEach(branch => {
        errors = errors.concat(checkExp(bvars.concat(fvarsOfPat(branch[0])), branch[1]))
        const dup = containsDups(fvarsOfPat(branch[0]))
        if (dup) {
          errors = errors.concat(repeatedVariableError(e, dup, branch[0].range))
        }
      })
      return errors
    }
  }
}

function checkProgram (bvars: string[], prog: Program): ErrorDetails[] {
  let errors: ErrorDetails[] = []
  prog.statements.forEach(s => {
    switch (s.tag) {
      case 'define':
        // if (bvars.includes(s.name.value)) {
        //   errors.push(shadowedVariableError(s.name.value, s.name.range))
        // }
        bvars = bvars.concat([s.name.value])
        errors = errors.concat(checkExp(bvars, s.value))
        return
      case 'struct': {
        const structBvars: Name[] = [
          name(s.id.value, s.id.range),
          name(`${s.id.value}?`, s.id.range),
          ...s.fields.map(f => name(`${s.id.value}-${f.value}`, f.range))
        ]
        // structBvars.forEach(x => {
        //   if (bvars.includes(x.value)) {
        //     errors.push(shadowedVariableError(x.value, x.range))
        //   }
        // })
        bvars = bvars.concat(structBvars.map(n => n.value))
        return
      }
      case 'exp':
        errors = errors.concat(checkExp(bvars, s.value))
        return
      case 'import':
        if (internalLibs.has(s.source)) {
          bvars = bvars.concat(Array.from(internalLibs.get(s.source)!.names()))
        }
    }
  })
  return errors
}

function mkInitialBVars (env: Env): string[] {
  return Array.from(env.names())
}

function scopeCheckExp (e: Exp, env: Env = preludeEnv): ErrorDetails[] {
  return checkExp(mkInitialBVars(env), e)
}

function scopeCheckProgram (prog: Program, env: Env = preludeEnv): ErrorDetails[] {
  return checkProgram(mkInitialBVars(env), prog)
}

export { scopeCheckExp, scopeCheckProgram }
