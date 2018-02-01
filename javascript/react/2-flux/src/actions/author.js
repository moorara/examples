const AuthorApi = require('../api/author') 
const Dispatcher = require('../dispatcher')
const ActionTypes = require('../constants/actions')

class AuthorActions {
  static getAuthors () {
    let authors = AuthorApi.getAuthors()
    Dispatcher.dispatch({
      type: ActionTypes.GET_AUTHORS,
      authors: authors
    })
  }

  static createAuthor (author) {
    let newAuthor = AuthorApi.createAuthor(author)
    Dispatcher.dispatch({
      type: ActionTypes.CREATE_AUTHOR,
      author: newAuthor
    })
  }

  static updateAuthor (author) {
    let updatedAuthor = AuthorApi.updateAuthor(author)
    Dispatcher.dispatch({
      type: ActionTypes.UPDATE_AUTHOR,
      author: updatedAuthor
    })
  }

  static deleteAuthor (authorId) {
    AuthorApi.deleteAuthor(authorId)
    Dispatcher.dispatch({
      type: ActionTypes.DELETE_AUTHOR,
      authorId: authorId
    })
  }
}

module.exports = AuthorActions
