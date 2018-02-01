import React from 'react'
import PropTypes from 'prop-types'
import { connect } from 'react-redux'
import { bindActionCreators } from 'redux'
import { withRouter } from 'react-router-dom'

import CourseList from './CourseList'
import * as courseActions from '../../actions/course'

class CoursesPage extends React.Component {
  constructor (props, context) {
    super(props, context)
    this.handleCourseAdd = this.handleCourseAdd.bind(this)
  }

  shouldComponentUpdate (nextProps, nextState, nextContext) {
    return true
  }

  courseItem (course, index) {
    return <div key={index}>{course.title}</div>
  }

  handleCourseAdd () {
    this.props.history.push('/course')
  }

  render () {
    const { courses } = this.props
    return (
      <div>
        <CourseList courses={courses} />
        <input type="submit" className="btn btn-primary" value="Add Course" onClick={this.handleCourseAdd} />
      </div>
    )
  }
}

CoursesPage.propTypes = {
  history: PropTypes.object.isRequired,
  courses: PropTypes.array.isRequired
}

function mapStateToProps (state, ownProps) {
  return {
    courses: state.courses
  }
}

function mapDispatchToProps (dispatch) {
  return {
    // createCourse: course => dispatch(courseActions.createCourse(course))
    actions: bindActionCreators(courseActions, dispatch)
  }
}

const connectToRedux = connect(mapStateToProps, mapDispatchToProps)

export default connectToRedux(withRouter(CoursesPage))
