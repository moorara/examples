import { combineReducers } from 'redux'

import authors from './author'
import courses from './course'
import apiCallsInProgress from './apiCall'

const rootReducer = combineReducers({
  authors,
  courses,
  apiCallsInProgress
})

export default rootReducer
