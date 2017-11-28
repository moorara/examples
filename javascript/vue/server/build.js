import path from 'path'
import rm from 'rimraf'
import chalk from 'chalk'
import webpack from 'webpack'

import webpackConfig from '../webpack.prod.config'

rm(path.resolve(__dirname, '../dist'), err => {
  if (err) throw err

  webpack(webpackConfig, (err, stats) => {
    if (err) throw err

    process.stdout.write(stats.toString({
      colors: true
    }) + '\n\n')

    if (stats.hasErrors()) {
      console.log(chalk.red('Build failed with errors.\n'))
      process.exit(1)
    }

    console.log(chalk.cyan('Build complete successfully.\n'))
  })
})
