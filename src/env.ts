/*
export type Entry = { value: Exp, source: string, range?: Range, documentation?: string }
export const entry = (value: Exp, source: string, range?: Range, documentation?: string): Entry =>
  ({ value, source, range, documentation })

export class Env {
  entries: Map<string, Entry>

  constructor (entries?: Iterable<[string, Entry]>) {
    if (entries) {
      this.entries = new Map(entries)
    } else {
      this.entries = new Map()
    }
  }

  public has (key: string): boolean {
    return this.entries.has(key)
  }

  public get (key: string): Entry | undefined {
    return this.entries.get(key)
  }

  public items (): Iterable<[string, Entry]> {
    return this.entries.entries()
  }

  public names (): Iterable<string> {
    return this.entries.keys()
  }

  public append (key: string, value: Entry): Env {
    return new Env([...this.entries, [key, value]])
  }

  public concat (other: Env): Env {
    return new Env([...this.entries, ...other.entries])
  }

  public without (keys: string[]): Env {
    const ret = new Env(this.items())
    keys.forEach(k => ret.entries.delete(k))
    return ret
  }
}
*/
