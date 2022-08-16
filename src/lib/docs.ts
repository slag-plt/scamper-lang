/**
 * A `Doc` is a convenience class for constructing docstrings for library
 * primitives.
 */
export class Doc {
  /**
   * 
   * @param sig A docstring corresponding to the signature of the function.
   * @param args An array of docstrings for each of the function's arguments.
   * @param desc A prose description of the behavior of the function.
   */
  constructor (public sig: string, public args: string[], public desc: string) { }

  /**
   * @returns A string containing the docstring formatted in Markdown.
   */
  public docToMarkdown(): string {
  return `
~~~
${this.sig.trim()}

${this.args.map(arg => '  ' + arg.trim()).join('\n')}
~~~

${this.desc.trim()}
  `.trim()
  }
}