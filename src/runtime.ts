import * as L from './lang.js'
import { Result, error } from './result.js'
import { msg } from './messages.js'

export function runtimeError <T> (message: string, s?: L.Exp, hint?: string): Result<T> {
  return s
    ? error(msg('phase-runtime'), message, s.range, L.expToString(s), hint)
    : error(msg('phase-runtime'), message, undefined, undefined, hint)
}

export function tryMatch (v: L.Value, p: L.Pat): L.Env | undefined {
  if (p.tag === 'wild') {
    return new L.Env([])
  } else if (p.tag === 'null' && L.valueIsNull(v)) {
    return new L.Env([])
  } else if (p.tag === 'var') {
    return new L.Env([[p.id, L.entry(v, 'match')]])
  } else if (p.tag === 'lit' && p.lit.tag === 'bool' && p.lit.value === v) {
    return new L.Env([])
  } else if (p.tag === 'lit' && p.lit.tag === 'num' && p.lit.value === v) {
    return new L.Env([])
  } else if (p.tag === 'lit' && p.lit.tag === 'str' && p.lit.value === v) {
    return new L.Env([])
  } else if (p.tag === 'lit' && p.lit.tag === 'char' && L.valueIsChar(v) && p.lit.value === (v as L.CharType).value) {
    return new L.Env([])
  } else if (p.tag === 'ctor' && (L.valueIsPair(v) || L.valueIsStruct(v))) {
    const head = p.head
    const args = p.args
    // Special cases: pairs are matched with either pair or cons
    if ((head === 'pair' || head === 'cons') && args.length === 2 && L.valueIsPair(v)) {
      const env1 = tryMatch((v as L.PairType).fst, args[0])
      const env2 = tryMatch((v as L.PairType).snd, args[1])
      return env1 && env2 ? env1.concat(env2) : undefined
    } else if (L.valueIsStructKind(v, head)) {
      const fields = [...((v as L.StructType).fields).values()]
      if (fields.length === args.length) {
        let env = new L.Env([])
        for (let i = 0; i < fields.length; i++) {
          const env2 = tryMatch(fields[i], args[i])
          if (!env2) {
            return undefined
          }
          env = env.concat(env2)
        }
        return env
      } else {
        // TODO: should we throw an error here?
        return undefined
      }
    } else {
      // N.B., at this point, we have run out of options, so the pattern match
      // has failed.
      return undefined
    }
  }
}

export class Store {
  private counter: number
  private readonly store: Map<number, L.Value>

  constructor () {
    this.counter = 0
    this.store = new Map()
  }

  add (value: L.Value): number {
    const key = this.counter++
    this.store.set(key, value)
    return key
  }

  get (key: number): L.Value | undefined {
    return this.store.get(key)
  }

  has (key: number): boolean {
    return this.store.has(key)
  }
}

export const store = new Store()
