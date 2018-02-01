const _ = require('lodash')
const uuid = require('uuid')
const authors = require('./data').authors

const _clone = (obj) => JSON.parse(JSON.stringify(obj))

class AuthorApi {
  static getAuthors () {
    return _clone(authors)
  }

  static getAuthor (id) {
    let author = _.find(authors, { id })
    return _clone(author)
  }

  static createAuthor (author) {
    console.log('Mocking backend-end ajax call for author creation')
    author.id = uuid.v4()
		authors.push(author)
    return _clone(author)
  }

  static updateAuthor (author) {
    console.log('Mocking backend-end ajax call for author updating')
    if (author.id) {
      let existingAuthor = _.find(authors, { id: author.id })
      let existingAuthorIndex = _.indexOf(authors, existingAuthor)
      authors.splice(existingAuthorIndex, 1, author)
      return _clone(author)
    }
  }

  static deleteAuthor (id) {
    console.log('Mocking backend-end ajax call for author deletion')
		_.remove(authors, { id })
  }
}

module.exports = AuthorApi
