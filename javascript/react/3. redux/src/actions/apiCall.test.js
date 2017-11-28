import 'should'

import * as types from './types'
import { beginApiCall, errorApiCall } from './apiCall'

describe('apiCallActions', () => {
  let action

  it('beginApiCall', () => {
    action = beginApiCall()
    action.type.should.equal(types.BEGIN_API_CALL)
  })
  it('errorApiCall', () => {
    action = errorApiCall()
    action.type.should.equal(types.ERROR_API_CALL)
  })
})
