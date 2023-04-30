const styleSheet = `
  /* PrismJS 1.29.0: https://prismjs.com/download.html#themes=prism&languages=markup+racket+scheme */
  code[class*=language-], pre[class*=language-] {
    color: #000;
    background: 0 0;
    text-shadow: 0 1px #fff;
    font-family: Menlo, Consolas, Monaco, Liberation Mono, Lucida Console, monospace;
    font-size: 1em;
    text-align: left;
    white-space: pre;
    word-spacing: normal;
    word-break: normal;
    word-wrap: normal;
    line-height: 1.5;
    -moz-tab-size: 4;
    -o-tab-size: 4;
    tab-size: 4;
    -webkit-hyphens: none;
    -moz-hyphens: none;
    -ms-hyphens: none;
    hyphens: none
  }
  code[class*=language-] ::-moz-selection, code[class*=language-]::-moz-selection, pre[class*=language-] ::-moz-selection, pre[class*=language-]::-moz-selection {
    text-shadow: none;
    background: #b3d4fc
  }
  code[class*=language-] ::selection,code[class*=language-]::selection,pre[class*=language-] ::selection,pre[class*=language-]::selection {
    text-shadow: none;
    background: #b3d4fc
  }
  @media print{
    code[class*=language-],pre[class*=language-] {
      text-shadow: none
    }
  }
  pre[class*=language-] {
    padding: 1em;
    margin: .5em 0;
    overflow: auto
  }
  :not(pre)>code[class*=language-],pre[class*=language-] {
    background: #f5f2f0
  }
  :not(pre)>code[class*=language-]{
    padding: .1em;
    border-radius: .3em;
    white-space: normal
  }
  .token.cdata, .token.comment, .token.doctype, .token.prolog {
    color: #708090
  }
  .token.punctuation {
    color: #999
  }
  .token.namespace {
    opacity: .7
  }
  .token.boolean, .token.constant, .token.deleted, .token.number, .token.property, .token.symbol, .token.tag {
    color:#905
  }
  .token.attr-name, .token.builtin, .token.char, .token.inserted, .token.selector, .token.string {
    color:#690
  }
  .language-css .token.string, .style .token.string, .token.entity, .token.operator, .token.url {
    color: #9a6e3a;
    background: hsla(0,0%,100%,.5)
  }
  .token.atrule, .token.attr-value, .token.keyword {
    color:#07a
  }
  .token.class-name, .token.function {
    color:#dd4a68
  }
  .token.important, .token.regex, .token.variable {
    color:#e90
  }
  .token.bold, .token.important {
    font-weight:700
  }
  .token.italic {
    font-style:italic
  }
  .token.entity {
    cursor:help
  }

  #header {
    background: #eee;
    color: #333;
    font-family: -apple-system, BlinkMacSystemFont, avenir next, avenir, segoe ui, helvetica neue, helvetica, Cantarell, Ubuntu, roboto, noto, arial, sans-serif;
    padding: 0.5em;
  }

  .output code {
    color: #000;
    font-family: Menlo, Consolas, Monaco, Liberation Mono, Lucida Console, monospace;
    font-size: 1em;
  }
`.trim()

/**
 * @param filename the name of the file being executed by the page.
 * @param version the version of Scamper being targetted.
 * @param src the Scamper source code being executed.
 * @returns a string that encompasses a complete, standalone Scamper webpage.
 */
export function makePage (filename: string, version: string, src: string): string {
  return `
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Scamper (${version}): ${filename}</title>
  <style>
${styleSheet}
  </style>
</head>
<body>
  <div id="header">
    Scamper (${version}): ${filename} ⋅
    <input type="checkbox" id="inline-source" name="inline-source" onclick="toggleInlineSource()">
    <label for="inline-source">Inline source</label>
  </div>
  <pre class="scamper-output">
${src}
  </pre>
  <script src="https://cdn.jsdelivr.net/npm/scamper-lang@${version}/dist/web/bundle.js" type="text/javascript"></script>
  <script>
    replaceCodeWidgets()
  </script>
</body>
</html>
  `.trim()
}
