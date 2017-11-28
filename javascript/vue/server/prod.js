import opn from 'opn'
import express from 'express'
import compression from 'compression'

import config from '../config'

const app = express()
const port = config.port

app.use(compression())
app.use(express.static('dist'))

app.listen(port, (err) => {
  if (err) {
    console.log(err)
  } else {
    opn(`http://localhost:${port}`)
  }
})
