import opn from 'opn'
import path from 'path'
import express from 'express'
import webpack from 'webpack'
import webpackDevMiddleware from 'webpack-dev-middleware'
import webpackHotMiddleware from 'webpack-hot-middleware'

import config from '../config'
import webpackConfig from '../webpack.dev.config'

const app = express()
const port = config.port
const compiler = webpack(webpackConfig)

app.use(webpackDevMiddleware(compiler, {
  noInfo: true,
  publicPath: webpackConfig.output.publicPath
}))

app.use(webpackHotMiddleware(compiler, {
  log: false,
  heartbeat: 1000
}))

app.get('*', (req, res) => {
  res.sendFile(path.join(__dirname, 'index.html'))
})

app.listen(port, (err) => {
  if (err) {
    console.log(err)
  } else {
    opn(`http://localhost:${port}`)
  }
})
