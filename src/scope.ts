import { ErrorDetails, errorDetails } from './result.js'
import { Env, Exp, Program } from './lang.js'
import { primMap } from './prims.js'
import { Range } from './loc.js'
import { msg } from './messages.js'

function undefinedVariableError (x: string, range: Range): ErrorDetails {
  return errorDetails(
    msg('phase-scope'),
    msg('error-var-undef', x),
    range,
    x
  )
}

function shadowedVariableError (x: string, range: Range): ErrorDetails {
  return errorDetails(
    msg('phase-scope'),
    msg('error-var-shadowed', x),
    range,
    x
  )
}

function checkExp (bvars: string[], e: Exp): ErrorDetails[] {
  switch (e.tag) {
    case 'var':
      return bvars.includes(e.value) ? [] : [undefinedVariableError(e.value, e.range)]
    case 'lit':
      return []
    case 'call':
      return checkExp(bvars, e.head).concat(e.args.flatMap((v) => checkExp(bvars, v)))
    case 'lam':
      return e.args.filter(x => bvars.includes(x.value))
        .map(x => shadowedVariableError(x.value, x.range))
        .concat(checkExp(bvars.concat(e.args.map(n => n.value)), e.body))
    case 'if':
      return checkExp(bvars, e.e1).concat(checkExp(bvars, e.e2).concat(checkExp(bvars, e.e3)))
    case 'nil':
      return []
    case 'pair':
      return checkExp(bvars, e.e1).concat(checkExp(bvars, e.e2))
    case 'let': {
      let errors: ErrorDetails[] = []
      e.bindings.forEach(binding => {
        if (bvars.includes(binding[0].value)) {
          errors.push(shadowedVariableError(binding[0].value, binding[0].range))
        }
        bvars = bvars.concat(binding[0].value)
        errors = errors.concat(checkExp(bvars, binding[1]))
      })
      return errors.concat(checkExp(bvars, e.body))
    }
    case 'cond':
      return e.branches.flatMap(b =>
        checkExp(bvars, b[0]).concat(checkExp(bvars, b[1])))
    case 'and':
      return e.args.flatMap((v) => checkExp(bvars, v))
    case 'or':
      return e.args.flatMap((v) => checkExp(bvars, v))
    case 'obj':
      return []
  }
}

function checkProgram (bvars: string[], prog: Program): ErrorDetails[] {
  let errors: ErrorDetails[] = []
  prog.statements.forEach(s => {
    switch (s.tag) {
      case 'define':
        if (bvars.includes(s.name.value)) {
          errors.push(shadowedVariableError(s.name.value, s.name.range))
        }
        bvars = bvars.concat([s.name.value])
        errors = errors.concat(checkExp(bvars, s.value))
        return
      case 'exp':
        errors = errors.concat(checkExp(bvars, s.value))
    }
  })
  return errors
}

function mkInitialBVars (env: Env): string[] {
  return Array.from(env.keys()).concat(Array.from(primMap.keys()))
}

function scopeCheckExp (env: Env, e: Exp): ErrorDetails[] {
  return checkExp(mkInitialBVars(env), e)
}

function scopeCheckProgram (prog: Program): ErrorDetails[] {
  return checkProgram(mkInitialBVars(new Map()), prog)
}

export { scopeCheckExp, scopeCheckProgram }
