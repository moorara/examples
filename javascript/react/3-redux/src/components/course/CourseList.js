import React from 'react'
import PropTypes from 'prop-types'

import CourseListItem from './CourseListItem'

const CourseList = ({ courses }) => {
  return (
    <div className="page">
      <h1>Courses</h1>
      <table className="table">
        <thead>
          <tr>
            <th>Watch</th>
            <th>Title</th>
            <th>Author</th>
            <th>Category</th>
            <th>Length</th>
          </tr>
        </thead>
        <tbody>
          {courses.map(course =>
            <CourseListItem key={course.id} course={course} />
          )}
        </tbody>
      </table>
    </div>
  )
}

CourseList.propTypes = {
  courses: PropTypes.array.isRequired
}

export default CourseList
