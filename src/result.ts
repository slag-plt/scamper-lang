import { Range } from './loc.js'

// TODO: at some point, plumb filenames throughout the entire system so we can report them!
type ErrorDetails = {
  phase: string,
  range?: Range
  message: string,
  src?: string
  hint?: string
}

const errorDetails = (phase: string, message: string, range?: Range, src?: string, hint?: string) => ({
  phase, range, message, src, hint
})

// TODO: make Error<T> carry an array of ErrorDetails so we can report multiple errors

type Error<T> = {
  tag: 'error',
  details: ErrorDetails[]
  // eslint-disable-next-line no-use-before-define
  andThen: <U>(f: (_:T) => Result<U>) => Result<U>
}

function detailsToCompleteString (details: ErrorDetails): string {
  const msg = [`:${details.range ? details.range.start.column : ''}:${details.range ? details.range.start.line : ''}: ${details.phase} error:`, `    ${details.message}`]
  if (details.hint) {
    msg.push('\n')
    msg.push(`    Hint: ${details.hint}`)
  }
  return msg.join('\n')
}

function detailsToMsgString (details: ErrorDetails): string {
  const msg = [`${details.phase} error: ${details.message}`]
  if (details.hint) {
    msg.push('\n')
    msg.push(`Hint: ${details.hint}`)
  }
  return msg.join('\n')
}

function errorToString <T> (err: Error<T>) {
  return err.details.map(detailsToCompleteString).join('\n')
}

type Ok<T> = {
  tag: 'ok',
  value: T
  // eslint-disable-next-line no-use-before-define
  andThen: <U>(f: (_:T) => Result<U>) => Result<U>
}

type Result<T> = Error<T> | Ok<T>

function errors <T> (errs: ErrorDetails[]): Error<T> {
  return { tag: 'error', details: errs, andThen: f => errors(errs) }
}

function error <T> (phase: string, message: string, range?: Range, src?: string, hint?: string): Error<T> {
  return errors([{
    phase,
    range,
    message,
    src,
    hint
  }])
}

function ok <T> (x: T): Ok<T> {
  return { tag: 'ok', value: x, andThen: f => f(x) }
}

function rethrow <T, U> (err: Error<T>) : Error<U> {
  // N.B., safe because Error's type variable is a phantom
  return err as any as Error<U>
}

function join <T> (arr: Result<T>[]): Result<T[]> {
  const result = []
  for (const e of arr) {
    switch (e.tag) {
      case 'error': return rethrow(e)
      case 'ok': result.push(e.value)
    }
  }
  return ok(result)
}

function detailsToResult (errs: ErrorDetails[]): Result<null> {
  return errs.length > 0 ? errors(errs) : ok(null)
}
class ICE extends Error {
  constructor (fn: string, reason: string, opts?: ErrorOptions) {
    super(`ICE (${fn}): ${reason}`, opts)
  }
}

export {
  Result, Error, ErrorDetails,
  errorDetails, error, errors, ok,
  detailsToCompleteString, detailsToMsgString, errorToString,
  rethrow, join, detailsToResult,
  ICE
}
