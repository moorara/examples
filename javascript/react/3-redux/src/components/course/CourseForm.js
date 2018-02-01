import React from 'react'
import PropTypes from 'prop-types'

import TextInput from '../common/TextInput'
import SelectInput from '../common/SelectInput'

const CourseForm = ({ course, authors, isSaving, onChange, onSave, errors }) => {
  return (
    <form>
      <TextInput
        name="title"
        label="Title"
        value={course.title}
        onChange={onChange}
        error={errors.title} />

      <SelectInput
        name="authorId"
        label="Author"
        value={course.authorId}
        defaultOption="Select Author"
        options={authors}
        onChange={onChange}
        error={errors.authorId} />

      <TextInput
        name="category"
        label="Category"
        value={course.category}
        onChange={onChange}
        error={errors.category} />

      <TextInput
        name="length"
        label="Length"
        value={course.length}
        onChange={onChange}
        error={errors.length} />

      <input
        type="submit"
        className="btn btn-primary"
        disabled={isSaving}
        value={isSaving ? 'Saving ...' : 'Save'}
        onClick={onSave} />
    </form>
  )
}

CourseForm.propTypes = {
  course: PropTypes.object.isRequired,
  authors: PropTypes.array.isRequired,
  isSaving: PropTypes.bool,
  onChange: PropTypes.func.isRequired,
  onSave: PropTypes.func.isRequired,
  errors: PropTypes.object
}

CourseForm.defaultProps = {
  isSaving: false,
  errors: []
}

export default CourseForm
