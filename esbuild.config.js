import esbuild from 'esbuild';
import babel from 'esbuild-plugin-babel';

esbuild
  .build({
    entryPoints: ['dist/web/index.js'],
    bundle: true,
    outfile: 'dist/web/bundle.js',
    plugins: [babel({
      filter: /.*/,
      namespace: '',
      config: {
        "plugins": [
          ["prismjs", {
              "languages": ["markup", "racket"],
              "plugins": [],
          }]
        ]
      }
    })],
      // target: ['es5'] // if you target es5 with babel, add this option
  })
  .catch(() => process.exit(1));