import 'babel-polyfill'

import './styles/styles.css'
import '../node_modules/bootstrap/dist/css/bootstrap.min.css'
import '../node_modules/toastr/build/toastr.min.css'

import React from 'react'
import ReactDOM from 'react-dom'
import { Provider } from 'react-redux'

import App from './components/App'
import configStore from './store/config'
import { loadAuthors } from './actions/author'
import { loadCourses } from './actions/course'

const store = configStore()
store.dispatch(loadAuthors())
store.dispatch(loadCourses())

ReactDOM.render((
  <Provider store={store}>
    <App />
  </Provider>
), document.getElementById('app'))
