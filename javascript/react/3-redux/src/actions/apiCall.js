import * as types from './types'

export function beginApiCall () {
  return {
    type: types.BEGIN_API_CALL
  }
}

export function errorApiCall () {
  return {
    type: types.ERROR_API_CALL
  }
}
