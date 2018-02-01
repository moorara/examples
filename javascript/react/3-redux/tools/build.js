import 'colors'
import webpack from 'webpack'

import webpackConfig from '../webpack.config.prod'

process.env.NODE_ENV = 'production'
console.log('Generating minified bundle for production via Webpack. This will take a moment...'.blue)

webpack(webpackConfig).run((err, stats) => {
  if (err) {
    console.log(err.bold.red)
    return 1
  }

  const jsonStats = stats.toJson()

  if (jsonStats.hasErrors) {
    return jsonStats.errors.map(err => console.log(err.red))
  }

  if (jsonStats.hasWarnings) {
    jsonStats.warnings.map(warning => console.log(warning.yellow))
  }

  console.log(`Webpack stats: ${stats}`)
  console.log('Your app has been compiled in production mode and written to /dist. It\'s ready to roll!'.green)

  return 0
})
