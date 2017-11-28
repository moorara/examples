import './styles/styles.css'

import Vue from 'vue'

import router from './router'
import store from './store'
import App from './components/App'
import { getMessages } from './store/actions'

/* eslint-disable no-new */
new Vue({
  el: '#app',
  router,
  store,
  render: h => h(App)
})

getMessages(store)
