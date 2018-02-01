import { createStore, applyMiddleware } from 'redux'
import thunk from 'redux-thunk'
import reduxImmutableStateInvariant from 'redux-immutable-state-invariant'

import rootReducer from '../reducers'

const middleware = process.env.NODE_ENV === 'production' ? [ thunk ] : [ reduxImmutableStateInvariant(), thunk ]

export default function configureStore (initialState) {
  return createStore(
    rootReducer,
    initialState,
    applyMiddleware(...middleware)
  )
}
