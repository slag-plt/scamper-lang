import { Doc } from '../lang.js'

export const equal: Doc = new Doc(
  '(equal? v1 v2): boolean', [
    'v1: any',
    'v2: any',
  ],
  'Returns `#t` if and only `v1` and `v2` are (structurally) equal values.'
)

export const number: Doc = new Doc(
  '(number? v): boolean', [
    'v: any'
  ],
  'Returns `#t` if and only `v` is a number.'
)

export const real: Doc = new Doc(
  '(real? v): boolean', [
    'v: any'
  ],
  'Returns `#t` if and only `v` is a real number.'
)

export const integer: Doc = new Doc(
  '(integer? v): boolean', [
    'v: any'
  ],
  'Returns `#t` if and only `v` is an integer.'
)
