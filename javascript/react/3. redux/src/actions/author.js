import * as types from './types'
import AuthorApi from '../api/author'
import { beginApiCall } from './apiCall'

export function loadAuthorsSuccess (authors) {
  return {
    type: types.LOAD_AUTHORS_SUCCESS,
    authors
  }
}

export function loadAuthors () {
  return dispatch => {
    dispatch(beginApiCall())

    return AuthorApi.getAuthors()
      .then(authors => dispatch(loadAuthorsSuccess(authors)))
      .catch(err => {
        throw err
      })
  }
}
