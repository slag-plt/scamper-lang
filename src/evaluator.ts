import * as L from './lang.js'
import { msg } from './messages.js'
import { runtimeError } from './runtime.js'
import { Result, ok, join, ICE, errorDetails } from './result.js'
import { imageLib } from './lib/image.js'
import { musicLib } from './lib/music.js'

export async function evalExp (env: L.Env, e: L.Exp): Promise<Result<L.Value>> {
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
            return runtimeError(msg('error-type-expected-call', e.head), e)
          }
        }))
    case 'lam':
      // TODO: need to close over the local environment here!
      return ok(L.vlambda(e.args, e.body))
    case 'if':
      return (await evalExp(env, e.e1)).asyncAndThen(async guard =>
        !L.valueIsBoolean(guard)
          ? runtimeError(msg('error-type-expected-cond', guard), e)
          : guard as boolean ? evalExp(env, e.e2) : evalExp(env, e.e3))
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
      while (e.branches.length > 0) {
        const [guard, body] = e.branches.shift()!
        const v = await evalExp(env, guard)
        if (v.tag === 'ok' && L.valueIsBoolean(v.value) && (v.value as boolean) === true) {
          return evalExp(env, body)
        }
      }
      return runtimeError(msg('error-cond-no-branch-applies'), e)
    case 'and':
      for (let i = 0; i < e.args.length; i++) {
        const v = await evalExp(env, e.args[i])
        if (v.tag === 'ok' && L.valueIsBoolean(v.value) && (v.value as boolean) === false) {
          return ok(false)
        }
      }
      return ok(true)
    case 'or':
      for (let i = 0; i < e.args.length; i++) {
        const v = await evalExp(env, e.args[i])
        if (v.tag === 'ok' && L.valueIsBoolean(v.value) && (v.value as boolean) === true) {
          return ok(true)
        }
      }
      return ok(false)
    case 'match':
      throw new Error('unimplemented!')
  }
}

export async function evalStmt (env: L.Env, s: L.Stmt): Promise<[L.Env, L.Stmt]> {
  switch (s.tag) {
    case 'error':
      return [env, s]
    case 'binding':
      return [env, s]
    case 'testresult':
      return [env, s]
    case 'value':
      return [env, s]
    case 'imported':
      return [env, s]
    case 'define': {
      const v = await evalExp(env, s.value)
      if (v.tag === 'ok') {
        return [
          env.append(s.name.value, L.entry(v.value, 'binding', s.name.range)),
          L.sbinding(s.name.value)]
      } else {
        return [env, L.serror(v.details)]
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
      return [
        env.concat(new L.Env([
          [name, L.entry(L.vprim(ctorPrim), `struct ${name}`, s.id.range)],
          [predName, L.entry(L.vprim(predPrim), `struct ${name}`, s.id.range)],
          ...fieldPrims
        ])),
        L.sbinding(`struct ${name}`)
      ]
    }
    case 'testcase': {
      throw new ICE('evalStmt', 'testcase unimplemented')
    }
    case 'exp': {
      const v = await evalExp(env, s.value)
      if (v.tag === 'ok') {
        return [env, L.svalue(v.value)]
      } else {
        return [env, L.serror(v.details)]
      }
    }
    case 'import': {
      // TODO: also copy-pasted from runtime.ts---refactor!
      if (internalLibs.has(s.source)) {
        return [
          new L.Env([...env.items(), ...internalLibs.get(s.source)!.items()]),
          L.simported(s.source)
        ]
      } else {
        return [
          env,
          L.serror([errorDetails(
            msg('phase-runtime'),
            msg('error-import-not-found', s.source, s.range)
          )])
        ]
      }
    }
  }
}

const internalLibs: Map<string, L.Env> = new Map([
  ['image', imageLib],
  ['music', musicLib]
])
