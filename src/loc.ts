export type Loc = { line: number, column: number, index: number }
export const mkLoc = (line: number, column: number, index: number): Loc => ({ line, column, index })
export const noLoc = () => mkLoc(-1, -1, -1)

export const locEquals = (l1: Loc, l2: Loc): boolean => l1.line === l2.line && l1.column === l2.column

export type Range = { start: Loc, end: Loc }
export const mkRange = (start: Loc, end: Loc): Range => ({ start, end })
export const noRange = () => mkRange(noLoc(), noLoc())

export const rangeEquals = (r1: Range, r2: Range): boolean => locEquals(r1.start, r2.start) && locEquals(r1.end, r2.end)

export function inRange (loc: Loc, range: Range): boolean {
  return (loc.line > range.start.line && loc.line < range.end.line) ||
         (loc.line === range.start.line && loc.column >= range.start.column) ||
         (loc.line === range.end.line && loc.column < range.end.column)
}
