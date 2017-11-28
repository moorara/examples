import React from 'react'
import ReactDOMServer from 'react-dom/server'
import axios from 'axios'

import config from '../config'
import StateApi from '../stateApi'
import App from '../components/App'

const serverRender = async () => {
  const resp = await axios.get(`http://${config.host}:${config.port}/data`)
  const store = new StateApi(resp.data)

  return {
    initialData: resp.data,
    initialMarkup: ReactDOMServer.renderToString(
      <App store={store} />
    )
  }
}

export default serverRender
