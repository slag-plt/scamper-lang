function formatWhitespace (text: string[]): string[] {
  const result = []
  let seenSpace = false
  for (let i = 0; i < text.length; i++) {
    if (text[i] === ' ' && !seenSpace) {
      seenSpace = true
      result.push(text[i])
    } else if (text[i] === ' ' && seenSpace) {
      continue
    } else {
      seenSpace = false
      result.push(text[i])
    }
  }
  return result
}

function findNextNonWhitespace (text: string[], i: number): number {
  while (i < text.length && [' ', '\t'].includes(text[i])) { i++ }
  return i
}

function formatIndentation (text: string[]): string[] {
  const result = []
  let level = 0
  for (let i = 0; i < text.length; i++) {
    if (text[i] === '\n') {
      // N.B., we "-1" to account for the i++ at the top of the loop.
      i = findNextNonWhitespace(text, i + 1) - 1
      result.push(`\n${' '.repeat(level >= 0 ? level * 2 : 0)}`)
    } else if (text[i] === '(' || text[i] === '[' || text[i] === '{') {
      level++
      result.push(text[i])
    } else if (text[i] === ')' || text[i] === ']' || text[i] === '}') {
      level--
      result.push(text[i])
    } else {
      result.push(text[i])
    }
  }
  return result
}

export function format (text: string): string {
  return formatIndentation(
    formatWhitespace(
      Array.from(text))).join('')
}
