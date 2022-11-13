export type Loc = { line: number, column: number }
export const mkLoc = (line: number, column: number): Loc => ({ line, column })
export const noLoc = () => mkLoc(-1, -1)

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

function allLineStartingPos (src: string): number[] {
  const result = [0]
  // N.B., the last character doesn't matter because if it is a newline
  // then it will not denote the start of a line.
  for (let i = 0; i < src.length - 1; i++) {
    if (src[i] === '\n') {
      result.push(i + 1)
    }
  }
  return result
}

export function locToIndex (lineIndices: number[], loc: Loc): number {
  // N.B., locs are 0-based for both line and column.
  return lineIndices[loc.line] + loc.column
}

export function splitByLocs (locs: Loc[], src: string): string[] {
  if (locs.length === 0) {
    return [src]
  } else {
    const lineIndices = allLineStartingPos(src)
    // N.B., throughout, need to be inclusive on the end index of one line/
    // the start index of the next.
    const result = [src.substring(0, locToIndex(lineIndices, locs[0]) + 1)]
    for (let i = 1; i < locs.length; i++) {
      result.push(src.substring(locToIndex(lineIndices, locs[i - 1]) + 1, locToIndex(lineIndices, locs[i]) + 1))
    }
    return result
  }
}
