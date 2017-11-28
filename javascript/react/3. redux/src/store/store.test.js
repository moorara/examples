import 'should'
import { createStore } from 'redux'

import rootReducer from '../reducers'
import initialState from '../reducers/initialState'
import * as actions from '../actions/course'

describe('Store', () => {
  let store

  beforeEach(() => {
    store = createStore(rootReducer, initialState)
  })

  it('loadCoursesSuccess should handle course loading', () => {
    const courses = [
      { id: '1234', title: 'ttd', url: 'https://ttd.code', authorId: 'abcd', length: '1:00' },
      { id: '5678', title: 'clean code', url: 'https://clean.code', authorId: 'abcd', length: '1:00' }
    ]
    const action = actions.loadCoursesSuccess(courses)
    store.dispatch(action)
    store.getState().courses.should.eql(courses)
  })
  it('createCourseSuccess should handle course creation', () => {
    const course = { id: '1234', title: 'ttd', url: 'https://ttd.code', authorId: 'abcd', length: '1:00' }
    const action = actions.createCourseSuccess(course)
    store.dispatch(action)
    store.getState().courses[0].should.eql(course)
  })
  it('updateCourseSuccess should handle course update', () => {
    const course = { id: '1234', title: 'ttd', url: 'https://ttd.code', authorId: 'abcd', length: '1:00' }
    const updatedCourse = { id: '1234', title: 'clean code', url: 'https://clean.code', authorId: 'abcd', length: '1:00' }
    const createAction = actions.createCourseSuccess(course)
    const updateAction = actions.updateCourseSuccess(updatedCourse)
    store.dispatch(createAction)
    store.dispatch(updateAction)
    store.getState().courses[0].should.eql(updatedCourse)
  })
})
