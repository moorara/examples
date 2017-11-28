import _ from 'lodash'
import toastr from 'toastr'
import React from 'react'
import PropTypes from 'prop-types'
import { connect } from 'react-redux'
import { bindActionCreators } from 'redux'

import CourseForm from './CourseForm'
import * as courseActions from '../../actions/course'

const MIN_TITLE_LEN = 5

export class ManageCoursePage extends React.Component {
  constructor (props, context) {
    super(props, context)
    this.state = {
      course: Object.assign({}, this.props.course),
      errors: {},
      isSaving: false
    }

    this.handleCourseUpdate = this.handleCourseUpdate.bind(this)
    this.handleCourseSave = this.handleCourseSave.bind(this)
  }

  componentWillReceiveProps (nextProps) {
    if (this.props.course.id !== nextProps.course.id) {
      // Needed when an existing course is loaded directly
      this.setState({
        course: Object.assign({}, nextProps.course)
      })
    }
  }

  shouldComponentUpdate (nextProps, nextState, nextContext) {
    return true
  }

  courseFormValid () {
    let isValid = true
    let errors = {}

    if (this.state.course.title.length < MIN_TITLE_LEN) {
      isValid = false
      errors.title = 'Title must be at least characters.'
    }

    this.setState({ errors })
    return isValid
  }

  handleCourseUpdate (event) {
    let course = this.state.course
    course[event.target.name] = event.target.value
    return this.setState({
      course: course
    })
  }

  handleCourseSave (event) {
    event.preventDefault()
    if (!this.courseFormValid()) {
      return
    }

    this.setState({ isSaving: true })
    this.props.actions.saveCourse(this.state.course)
      .then(() => {
        toastr.success('Course saved.')
        this.context.router.history.push('/courses')
      })
      .catch(err => {
        toastr.error(err)
      })
      .finally(() => {
        this.setState({ isSaving: false })
      })
  }

  render () {
    return (
      <div>
        <h1>Manage Course</h1>
        <CourseForm
          course={this.state.course}
          authors={this.props.authors}
          isSaving={this.state.isSaving}
          onChange={this.handleCourseUpdate}
          onSave={this.handleCourseSave}
          errors={this.state.errors} />
      </div>
    )
  }
}

ManageCoursePage.propTypes = {
  course: PropTypes.object.isRequired,
  authors: PropTypes.array.isRequired,
  actions: PropTypes.object.isRequired
}

ManageCoursePage.contextTypes = {
  router: PropTypes.object
}

export function mapStateToProps (state, ownProps) {
  const courseId = ownProps.match.params.id // form /course/:id
  let course =
    courseId && state.courses.length > 0 ?
    _.find(state.courses, course => course.id === courseId) :
    { id: '', title: '', url: '', authorId: '', length: '', category: '' }

  let authorsFormatted = state.authors.map(author => ({
    value: author.id,
    text: `${author.firstName} ${author.lastName}`
  }))

  return {
    course,
    authors: authorsFormatted
  }
}

export function mapDispatchToProps (dispatch) {
  return {
    actions: bindActionCreators(courseActions, dispatch)
  }
}

export default connect(mapStateToProps, mapDispatchToProps)(ManageCoursePage)
