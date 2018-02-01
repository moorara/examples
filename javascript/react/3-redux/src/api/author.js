/*
 * This file mocks Author APIs. It uses setTimeout to simulate the delay of AJAX calls.
 * All calls return promises.
 */

import Promise from 'bluebird'
import uuidv4 from 'uuid/v4'

const DELAY = 1000
const MIN_NAME_LEN = 3

const authors = [
  {
    id: 'cory-house',
    firstName: 'Cory',
    lastName: 'House'
  },
  {
    id: 'dijkstra',
    firstName: 'Edsger',
    lastName: 'Dijkstra'
  },
  {
    id: 'akay',
    firstName: 'Alan',
    lastName: 'Kay'
  },
  {
    id: 'moorara',
    firstName: 'Milad',
    lastName: 'Irannejad'
  }
]

class AuthorApi {
  static getAuthors () {
    return new Promise((resolve, reject) => {
      setTimeout(() => {
        resolve(Object.assign([], authors))
      }, DELAY)
    })
  }

  static createAuthor (author) {
    author = Object.assign({}, author)
    return new Promise((resolve, reject) => {
      setTimeout(() => {
        if (author.firstName.length < MIN_NAME_LEN) {
          reject(new Error(`First Name must be at least ${MIN_NAME_LEN} characters.`))
        } else if (author.lastName.length < MIN_NAME_LEN) {
          reject(new Error(`Last Name must be at least ${MIN_NAME_LEN} characters.`))
        }

        author.id = uuidv4()
        authors.push(author)
        resolve(author)
      }, DELAY)
    })
  }

  static updateAuthor (author) {
    author = Object.assign({}, author)
    return new Promise((resolve, reject) => {
      setTimeout(() => {
        if (!author.id) {
          reject(new Error('Author id is missing.'))
        } else if (author.firstName.length < MIN_NAME_LEN) {
          reject(new Error(`First Name must be at least ${MIN_NAME_LEN} characters.`))
        } else if (author.lastName.length < MIN_NAME_LEN) {
          reject(new Error(`Last Name must be at least ${MIN_NAME_LEN} characters.`))
        }

        const authorIndex = authors.findIndex(a => a.id === author.id)
        authors.splice(authorIndex, 1, author)
        resolve(author)
      }, DELAY)
    })
  }

  static deleteAuthor (authorId) {
    return new Promise((resolve, reject) => {
      setTimeout(() => {
        const authorIndex = authors.findIndex(author => author.id === authorId)
        authors.splice(authorIndex, 1)
        resolve()
      }, DELAY)
    })
  }
}

export default AuthorApi
