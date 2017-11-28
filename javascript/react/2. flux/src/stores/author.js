const _ = require('lodash')
const EventEmitter = require('events').EventEmitter

const Dispatcher = require('../dispatcher')
const ActionTypes = require('../constants/actions')

const CHANGE_EVENT = 'change'

let _authors = []

class AuthorStore extends EventEmitter {
  constructor () {
    super()
    this.dispatchToken = Dispatcher.register(action => {
      switch (action.type) {
        case ActionTypes.GET_AUTHORS:
          _authors = action.authors
          this.emitChange()
          break
        case ActionTypes.CREATE_AUTHOR:
          _authors.push(action.author)
          this.emitChange()
          break
        case ActionTypes.UPDATE_AUTHOR:
          _authors = _.unionBy([ action.author ], _authors, 'id')
          this.emitChange()
          break
        case ActionTypes.DELETE_AUTHOR:
          _.remove(_authors, (author) => author.id === action.authorId)
          this.emitChange()
          break
      }
    })
  }

  addChangeListener (callback) {
    this.on(CHANGE_EVENT, callback)
  }

  removeChangeListener (callback) {
    this.removeListener(CHANGE_EVENT, callback)
  }

  emitChange () {
    this.emit(CHANGE_EVENT)
  }

  getAuthors () {
    return _authors
  }

  getAuthor (id) {
    return _.find(_authors, { id })
  }
}

module.exports = new AuthorStore()
