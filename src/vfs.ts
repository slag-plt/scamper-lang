import { Result, error, ok, ICE } from './result.js'
import { msg } from './messages.js'

export interface VFSProvider {
  read(path: string): Promise<Result<string>>;
  write(path: string, content: string): Promise<Result<void>>;
}

export function fileNotFoundError (path: string): Result<any> {
  return error(msg('phase-runtime'), msg('error-file-not-found', path))
}

export class InMemoryProvider implements VFSProvider {
  files: Map<string, string>

  constructor (files: [string, string][]) {
    this.files = new Map(files)
  }

  read (path: string): Promise<Result<string>> {
    if (this.files.has(path)) {
      return Promise.resolve(ok(this.files.get(path)!))
    } else {
      return Promise.resolve(fileNotFoundError(path))
    }
  }

  write (path: string, content: string): Promise<Result<void>> {
    throw new ICE('VFS.write', 'not implemented')
  }
}

class VFS {
  // A mapping from path prefixes to providers that service files under that path.
  mountPoints: Map<string, VFSProvider>

  constructor () {
    this.mountPoints = new Map()
  }

  mount (path: string, provider: VFSProvider): void {
    this.mountPoints.set(path, provider)
  }

  async read (path: string): Promise<Result<string>> {
    for (const [prefix, provider] of this.mountPoints.entries()) {
      if (path.startsWith(prefix)) {
        try {
          // N.B., await here to force synchronization so we can catch a
          // rejected promise at this point.
          return await provider.read(path.substring(prefix.length))
        } catch (_) {
          // TODO: we'll interpret a rejected promise as a file not being found.
          // We'll want to perform more fine-grained error handling here.
          return fileNotFoundError(path)
        }
      }
    }
    return fileNotFoundError(path)
  }

  write (path: string, content: string): Result<void> {
    throw new ICE('VFS.write', 'not implemented')
  }
}

export const fs = new VFS()
