import { Result, error, ok, ICE } from './result.js'
import { msg } from './messages.js'

export interface VFSProvider {
  read(path: string): Result<string>;  
  write(path: string, content: string): Result<void>;
}

export function fileNotFoundError(path: string): Result<any> {
  return error(msg('phase-runtime'), msg('error-file-not-found', path))
}

export class InMemoryProvider implements VFSProvider {
  files: Map<string, string>

  constructor (files: [string, string][]) {
    this.files = new Map(files)
  }

  read(path: string): Result<string> {
    if (this.files.has(path)) {
      return ok(this.files.get(path)!)
    } else {
      return fileNotFoundError(path)
    }
  }
  write(path: string, content: string): Result<void> {
    throw new ICE('VFS.write', 'not implemented')
  }
}

class VFS {
  // A mapping from path prefixes to providers that service files under that path.
  mountPoints: Map<string, VFSProvider>;

  constructor() {
    this.mountPoints = new Map();
  }

  mount(path: string, provider: VFSProvider): void {
    this.mountPoints.set(path, provider);
  }

  read(path: string): Result<string> {
    // TODO: hack to quickly get in https pull-down support for files!
    if (path.startsWith('http://') || path.startsWith('https://')) {
      console.log('...')
      const request = new XMLHttpRequest();
      request.open('GET', path, false);  // `false` makes the request synchronous
      request.send(null);
      if (request.status === 200) {
        console.log(request.responseText)
        return ok(request.responseText);
      } else {
        throw new ICE('VFS.read', 'request didnt work!')
      }
    }

    for (const [prefix, provider] of this.mountPoints.entries()) {
      if (path.startsWith(prefix)) {
        return provider.read(path.substring(prefix.length));
      }
    }
    return fileNotFoundError(path)
  }

  write(path: string, content: string): Result<void> {
    throw new ICE('VFS.write', 'not implemented')
  }
}

export const fs = new VFS()