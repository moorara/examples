const React = require('react')
const PropTypes = require('prop-types')

class TextInput extends React.Component {
  render () {
    const { name, label, onChange, placeholder, value, error } = this.props

    let wrapperClass = 'form-group'
    if (error && error.length > 0) {
      wrapperClass += ' has-error'
    }

    return (
      <div className={wrapperClass}>
        <label htmlFor="name">{label}</label>
        <div className="field">
          <input type="text"
            name={name}
            className="form-control"
            placeholder={placeholder}
            ref={name}
            value={value}
            onChange={onChange} />
            <div className="input" style={{ color: 'red', fontSize: 11 }}>{error}</div>
        </div>
      </div>
    )
  }
}

TextInput.propTypes = {
  name: PropTypes.string.isRequired,
  label: PropTypes.string.isRequired,
  onChange: PropTypes.func.isRequired,
  plaveholder: PropTypes.string,
  value: PropTypes.string,
  error: PropTypes.string
}

module.exports = TextInput
