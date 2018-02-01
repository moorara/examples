import 'should'
import nock from 'nock'
import thunk from 'redux-thunk'
import configureStore from 'redux-mock-store'

import * as types from './types'
import { loadCourses, saveCourse } from './course'
import { loadCoursesSuccess, createCourseSuccess, updateCourseSuccess } from './course'

describe('courseActions', () => {
  let action, course, courses

  before(() => {
    course = { id: 'aaaa', title: 'react', url: 'https://reactjs.org', authorId: '1111', length: '1:00' }
    courses = [
      course,
      { id: 'bbbb', title: 'redux', url: 'http://redux.js.org', authorId: '2222', length: '1:00' }
    ]
  })

  it('loadCoursesSuccess', () => {
    action = loadCoursesSuccess(courses)
    action.type.should.equal(types.LOAD_COURSES_SUCCESS)
    action.courses.should.eql(courses)
  })
  it('createCourseSuccess', () => {
    action = createCourseSuccess(course)
    action.type.should.equal(types.CREATE_COURSE_SUCCESS)
    action.course.should.eql(course)
  })
  it('updateCourseSuccess', () => {
    action = updateCourseSuccess(course)
    action.type.should.equal(types.UPDATE_COURSE_SUCCESS)
    action.course.should.eql(course)
  })
})

describe('courseThunks', () => {
  const middleware = [ thunk ]
  const mockStore = configureStore(middleware)

  afterEach(() => {
    nock.cleanAll()
  })

  it('loadCourses should create BEGIN_API_CALL and LOAD_COURSES_SUCCESS', (done) => {
    const store = mockStore({ courses: [] })
    store.dispatch(loadCourses()).then(() => {
      const actions = store.getActions()
      actions[0].type.should.equal(types.BEGIN_API_CALL)
      actions[1].type.should.equal(types.LOAD_COURSES_SUCCESS)
      done()
    })
  })
  it('saveCourse should create BEGIN_API_CALL and CREATE_COURSE_SUCCESS', (done) => {
    const store = mockStore({ courses: [] })
    store.dispatch(saveCourse({ title: 'redux' })).then(() => {
      const actions = store.getActions()
      actions[0].type.should.equal(types.BEGIN_API_CALL)
      actions[1].type.should.equal(types.CREATE_COURSE_SUCCESS)
      done()
    })
  })
  it('saveCourse should create BEGIN_API_CALL and UPDATE_COURSE_SUCCESS', (done) => {
    const store = mockStore({ courses: [] })
    store.dispatch(saveCourse({ id: '1234', title: 'redux' })).then(() => {
      const actions = store.getActions()
      actions[0].type.should.equal(types.BEGIN_API_CALL)
      actions[1].type.should.equal(types.UPDATE_COURSE_SUCCESS)
      done()
    })
  })
})
