import React from 'react'
import PropTypes from 'prop-types'

const SelectInput = ({ name, label, value, defaultOption, options, onChange, error }) => {

  return (
    <div className="form-group">
      <label htmlFor={name}>{label}</label>
      <div className="field">
        <select name={name} className="form-control" value={value} onChange={onChange}>
          <option value="">{defaultOption}</option>
          {options.map(option =>
            <option key={option.value} value={option.value}>{option.text}</option>
          )}
        </select>
        {error && <div className="alert alert-danger">{error}</div>}
      </div>
    </div>
  )
}

SelectInput.propTypes = {
  name: PropTypes.string.isRequired,
  label: PropTypes.string.isRequired,
  value: PropTypes.string,
  defaultOption: PropTypes.string,
  options: PropTypes.arrayOf(PropTypes.object),
  onChange: PropTypes.func.isRequired,
  error: PropTypes.string
}

SelectInput.defaultProps = {
  value: '',
  defaultOption: '',
  options: [],
  error: ''
}

export default SelectInput
