import * as L from './lang.js'
import { msg } from './messages.js'
import { runtimeError, tryMatch } from './runtime.js'
import { Result, ok, join, errorDetails } from './result.js'
import * as Libs from './lib/exports.js'

export async function evaluateExp (env: L.Env, e: L.Exp): Promise<Result<L.Value>> {
  switch (e.tag) {
    case 'value':
      return Promise.resolve(ok(e.value))
    case 'var':
      return Promise.resolve(env.has(e.value)
        ? ok(env.get(e.value)!.value)
        : runtimeError(msg('error-var-undef', e.value), e))
    case 'lit':
      return ok(L.litToValue(e.value))
    case 'call':
      return (await evaluateExp(env, e.head)).asyncAndThen(async head =>
        join(await Promise.all(e.args.map(arg => evaluateExp(env, arg)))).asyncAndThen(async args => {
          if (L.valueIsLambda(head)) {
            const lam = head as L.LambdaType
            return args.length === lam.args.length
              ? evaluateExp(new L.Env(lam.args.map((x, i) => [x.value, L.entry(args[i], 'local')]), lam.env), lam.body)
              : runtimeError(msg('error-arity', 'lambda', lam.args.length, args.length), e)
          } else if (L.valueIsPrim(head)) {
            return await (head as L.PrimType).fn(env, args, e)
          } else {
            return runtimeError(msg('error-type-expected-call', e.head), e)
          }
        }))
    case 'lam':
      return ok(L.vlambda(e.args, e.body, env))
    case 'if':
      return (await evaluateExp(env, e.e1)).asyncAndThen(async guard =>
        !L.valueIsBoolean(guard)
          ? runtimeError(msg('error-type-expected-cond', guard), e)
          : guard as boolean ? evaluateExp(env, e.e2) : evaluateExp(env, e.e3))
    case 'nil':
      return ok(null)
    case 'pair':
      // TODO: could optimize this for lists to avoid excessive recursion,
      // but the problem of deep recursion still remains.
      return (await evaluateExp(env, e.e1)).asyncAndThen(async v1 =>
        (await evaluateExp(env, e.e2)).asyncAndThen(v2 =>
          Promise.resolve(ok(L.vpair(v1, v2)))))
    case 'let':
      if (e.kind === 'let') {
        const bindings: [string, L.EnvEntry][] = []
        for (let i = 0; i < e.bindings.length; i++) {
          const [x, body] = e.bindings[i]
          const v = await evaluateExp(env, body)
          if (v.tag === 'ok') {
            bindings.push([x.value, L.entry(v.value, 'local')])
          } else {
            return v
          }
        }
        return evaluateExp(new L.Env(bindings, env), e.body)
      } else if (e.kind === 'let*') {
        for (let i = 0; i < e.bindings.length; i++) {
          const [x, body] = e.bindings[i]
          const v = await evaluateExp(env, body)
          if (v.tag === 'ok') {
            env = new L.Env([[x.value, L.entry(v.value, 'local')]], env)
          } else {
            return v
          }
        }
        return evaluateExp(env, e.body)
      } else {
        throw new Error('letrec unimplemented')
      }
    case 'cond':
      for (let i = 0; i < e.branches.length; i++) {
        const [guard, body] = e.branches[i]
        const v = await evaluateExp(env, guard)
        if (v.tag === 'ok' && L.valueIsBoolean(v.value) && (v.value as boolean) === true) {
          return evaluateExp(env, body)
        }
      }
      return runtimeError(msg('error-cond-no-branch-applies'), e)
    case 'and':
      for (let i = 0; i < e.args.length; i++) {
        const v = await evaluateExp(env, e.args[i])
        if (v.tag === 'ok' && L.valueIsBoolean(v.value) && (v.value as boolean) === false) {
          return ok(false)
        }
      }
      return ok(true)
    case 'or':
      for (let i = 0; i < e.args.length; i++) {
        const v = await evaluateExp(env, e.args[i])
        if (v.tag === 'ok' && L.valueIsBoolean(v.value) && (v.value as boolean) === true) {
          return ok(true)
        }
      }
      return ok(false)
    case 'match': {
      return (await evaluateExp(env, e.scrutinee)).asyncAndThen(async scrutinee => {
        for (let i = 0; i < e.branches.length; i++) {
          const [pat, body] = e.branches[i]
          const env2 = tryMatch(scrutinee, pat)
          if (env2) {
            // TODO: env chain instead of concat? Does it matter?
            return evaluateExp(new L.Env(env2.items(), env), body)
          }
        }
        return runtimeError(msg('error-match-no-branch-applies'), e)
      })
    }
  }
}

export async function evaluateStmt (env: L.Env, s: L.Stmt): Promise<L.Stmt> {
  switch (s.tag) {
    case 'error':
      return s
    case 'binding':
      return s
    case 'testresult':
      return s
    case 'value':
      return s
    case 'imported':
      return s
    case 'define': {
      const v = await evaluateExp(env, s.value)
      if (v.tag === 'ok') {
        env.set(s.name.value, L.entry(v.value, 'binding', s.name.range))
        return L.sbinding(s.name.value)
      } else {
        return L.serror(v.details)
      }
    }
    case 'struct': {
      // TODO: this is copy-pasted from runtime.ts---probably should refactor!
      const name = s.id.value
      const predName = `${name}?`
      // primitive for type-testing predicate: id?
      const predPrim: L.Prim = (env, args, app) =>
        Promise.resolve(args.length !== 1
          ? runtimeError(msg('error-arity', predName, 1, args.length), app)
          : ok(L.valueIsStructKind(args[0], name)))
      // field-accessing primitives: id-field?
      const fieldPrims: [string, L.EnvEntry][] = s.fields.map((f, i) => {
        const fieldName: string = `${name}-${f.value}`
        return [fieldName, L.entry(
          L.vprim((_env, args, app) =>
            Promise.resolve(args.length !== 1
              ? runtimeError(msg('error-arity', fieldName, 1, args.length), app)
              : !L.valueIsStructKind(args[0], name)
                  ? runtimeError(msg('error-type-expected-fun', 1, fieldName, `struct ${name}`, args[0]))
                  // N.B., fields are encoded positionally. Field names exist purely at the source level.
                  : ok((((args[0] as L.StructType).fields[i]))))),
          `struct ${name}`,
          f.range
        )]
      })
      // constructor primitive: id
      const ctorPrim: L.Prim = (env, args, app) => {
        if (args.length !== s.fields.length) {
          return Promise.resolve(runtimeError(msg('error-arity', name, s.fields.length, args.length), app))
        } else {
          return Promise.resolve(ok(L.vstruct(name, [...args])))
        }
      }
      env.setAll([
        [name, L.entry(L.vprim(ctorPrim), `struct ${name}`, s.id.range)],
        [predName, L.entry(L.vprim(predPrim), `struct ${name}`, s.id.range)],
        ...fieldPrims
      ])
      return L.sbinding(`struct ${name}`)
    }
    case 'testcase': {
      // TODO: also taken verbatim from runtime.ts---refactor!
      const result: Result<L.Stmt> = await (await evaluateExp(env, s.desc)).asyncAndThen(async e1 =>
        (await evaluateExp(env, s.expected)).asyncAndThen(async expected =>
          (await evaluateExp(env, s.actual)).asyncAndThen(async actual =>
            (await evaluateExp(env, L.nlecall(s.comp, [L.nlevalue(expected), L.nlevalue(actual)]))).andThen(e2 => {
              if (!L.valueIsString(e1)) {
                return runtimeError(msg('error-type-expected', 'string', e1), s.desc)
              } else if (!L.valueIsBoolean(e2)) {
                return runtimeError(msg('error-type-expected', 'bool', e2), s.comp)
              }
              const desc = e1 as string
              const passed = e2 as boolean
              return ok(passed
                ? L.stestresult(desc, true)
                // TODO: should probably work with values rather than exps here
                : L.stestresult(desc, false, undefined, L.nlevalue(expected), L.nlevalue(actual)))
            }))))
      if (result.tag === 'error') {
        return L.serror(result.details)
      } else {
        return result.value
      }
    }
    case 'exp': {
      const v = await evaluateExp(env, s.value)
      if (v.tag === 'ok') {
        return L.svalue(v.value)
      } else {
        return L.serror(v.details)
      }
    }
    case 'import': {
      // TODO: also copy-pasted from runtime.ts---refactor!
      if (Libs.internalLibs.has(s.source)) {
        // TODO: mutate environment entries, instead of creating a new one
        env.setAll([...env.items(), ...Libs.internalLibs.get(s.source)!.items()])
        return L.simported(s.source)
      } else {
        return L.serror([errorDetails(
          msg('phase-runtime'),
          msg('error-import-not-found', s.source, s.range))])
      }
    }
  }
}

export async function evaluateProgram (prog: L.Program, initialEnv: L.Env = Libs.preludeEnv): Promise<L.SEffect[]> {
  const results: L.SEffect[] = []
  const env = new L.Env(initialEnv.entries)
  for (let i = 0; i < prog.length; i++) {
    const result = await evaluateStmt(env, prog[i])
    results.push(result as L.SEffect)
  }
  return results
}
