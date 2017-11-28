import * as types from './types'
import CourseApi from '../api/course'
import { beginApiCall, errorApiCall } from './apiCall'

export function loadCoursesSuccess (courses) {
  return {
    type: types.LOAD_COURSES_SUCCESS,
    courses
  }
}

export function createCourseSuccess (course) {
  return {
    type: types.CREATE_COURSE_SUCCESS,
    course
  }
}

export function updateCourseSuccess (course) {
  return {
    type: types.UPDATE_COURSE_SUCCESS,
    course
  }
}

export function loadCourses () {
  return dispatch => {
    dispatch(beginApiCall())

    return CourseApi.getCourses()
      .then(courses => dispatch(loadCoursesSuccess(courses)))
      .catch(err => {
        throw err
      })
  }
}

export function saveCourse (course) {
  return (dispatch, getState) => {
    dispatch(beginApiCall())

    const saveCourse = course.id ? CourseApi.updateCourse : CourseApi.createCourse
    const saveCourseSuccess = course.id ? updateCourseSuccess : createCourseSuccess
    return saveCourse(course)
      .then(course => dispatch(saveCourseSuccess(course)))
      .catch(err => {
        dispatch(errorApiCall(err))
        throw err
      })
  }
}
