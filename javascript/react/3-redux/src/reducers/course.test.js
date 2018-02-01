import 'should'

import courseReducer from './course'
import * as actions from '../actions/course'

describe('courseReducer', () => {
  let state, course, courses

  before(() => {
    state = [
      { id: 'aaaa', title: 'react' },
      { id: 'bbbb', title: 'flux' }
    ]
    course = { id: 'cccc', title: 'redux' }
    courses = [
      course = { id: 'dddd', title: 'mobx' },
      course = { id: 'eeee', title: 'tide' }
    ]
  })

  it('should load courses when passed LOAD_COURSES_SUCCESS', () => {
    const action = actions.loadCoursesSuccess(courses)
    const newState = courseReducer(state, action)
    newState[0].should.eql(state[0])
    newState[1].should.eql(state[1])
    newState[2].should.eql(courses[0])
    newState[3].should.eql(courses[1])
  })
  it('should create course when passed CREATE_COURSE_SUCCESS', () => {
    const action = actions.createCourseSuccess(course)
    const newState = courseReducer(state, action)
    newState[0].should.eql(state[0])
    newState[1].should.eql(state[1])
    newState[2].should.eql(course)
  })
  it('should update course when passed UPDATE_COURSE_SUCCESS', () => {
    course.id = 'bbbb'
    const action = actions.updateCourseSuccess(course)
    const newState = courseReducer(state, action)
    newState[0].should.eql(state[0])
    newState[1].should.eql(course)
  })
})
