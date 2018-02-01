import * as types from '../actions/types'
import initialState from './initialState'

export default function apiCallReducer (state = initialState.apiCallsInProgress, action) {
  if (action.type === types.BEGIN_API_CALL) {
    return state + 1
  } else if (action.type === types.ERROR_API_CALL || action.type.substring(action.type.length - 8) === '_SUCCESS') {
    return state - 1
  }

  return state
}
