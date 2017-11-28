import 'should'

import * as types from './types'
import { loadAuthorsSuccess } from './author'

describe('authorActions', () => {
  let action, authors

  before(() => {
    authors = [{ id: '1111', firstName: 'Dan', lastName: 'Abramov' }]
  })

  it('loadAuthorsSuccess', () => {
    action = loadAuthorsSuccess(authors)
    action.type.should.equal(types.LOAD_AUTHORS_SUCCESS)
    action.authors.should.eql(authors)
  })
})
