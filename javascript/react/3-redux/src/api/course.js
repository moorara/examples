/*
 * This file mocks Author APIs. It uses setTimeout to simulate the delay of AJAX calls.
 * All calls return promises.
 */

import Promise from 'bluebird'
import uuidv4 from 'uuid/v4'

const DELAY = 1000
const MIN_TITLE_LEN = 5

const courses = [
  {
    id: 'writing-clean-code-humans',
    title: 'Clean Code: Writing Code for Humans',
    url: 'http://www.pluralsight.com/courses/writing-clean-code-humans',
    authorId: 'cory-house',
    length: '3:10',
    category: 'Software Engineering'
  },
  {
    id: 'react-redux-react-router-es6',
    title: 'Building Applications with React and Redux in ES6',
    url: 'http://www.pluralsight.com/courses/react-redux-react-router-es6',
    authorId: 'cory-house',
    length: '6:13',
    category: 'JavaScript'
  },
  {
    id: 'react-flux-building-applications',
    title: 'Building Applications with React and Flux',
    url: 'http://www.pluralsight.com/courses/react-flux-building-applications',
    authorId: 'cory-house',
    length: '5:08',
    category: 'JavaScript'
  },
  {
    id: 'web-components-shadow-dom',
    title: 'HTML5 Web Component Fundamentals',
    url: 'http://www.pluralsight.com/courses/web-components-shadow-dom',
    authorId: 'cory-house',
    length: '5:03',
    category: 'HTML5'
  }
]

class CourseApi {
  static getCourses () {
    return new Promise((resolve, reject) => {
      setTimeout(() => {
        resolve(Object.assign([], courses))
      }, DELAY)
    })
  }

  static createCourse (course) {
    course = Object.assign({}, course)
    return new Promise((resolve, reject) => {
      setTimeout(() => {
        if (course.title.length < MIN_TITLE_LEN) {
          reject(new Error(`Title must be at least ${MIN_TITLE_LEN} characters.`))
        }

        course.id = uuidv4()
        course.url = `http://www.pluralsight.com/courses/${course.id}`
        courses.push(course)
        resolve(course)
      }, DELAY)
    })
  }

  static updateCourse (course) {
    course = Object.assign({}, course)
    return new Promise((resolve, reject) => {
      setTimeout(() => {
        if (!course.id) {
          reject(new Error('Course id is missing.'))
        } else if (course.title.length < MIN_TITLE_LEN) {
          reject(new Error(`Title must be at least ${MIN_TITLE_LEN} characters.`))
        }

        const courseIndex = courses.findIndex(c => c.id === course.id)
        courses.splice(courseIndex, 1, course)
        resolve(course)
      }, DELAY)
    })
  }

  static deleteCourse (courseId) {
    return new Promise((resolve, reject) => {
      setTimeout(() => {
        let courseIndex = courses.findIndex(course => course.id === courseId)
        courses.splice(courseIndex, 1)
        resolve()
      }, DELAY)
    })
  }
}

export default CourseApi
