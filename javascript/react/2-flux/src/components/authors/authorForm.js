const React = require('react')
const PropTypes = require('prop-types')
const Prompt = require('react-router-dom').Prompt

const TextInput = require('../common/textInput')

const AuthorForm = (props) => (
  <form>
    <TextInput type="text"
      name="firstname"
      label="First Name"
      value={props.author.firstname}
      onChange={props.updateAuthor}
      error={props.errors.firstname} />

    <TextInput type="text"
      name="lastname"
      label="Last Name"
      value={props.author.lastname}
      onChange={props.updateAuthor}
      error={props.errors.lastname} />

    <input type="submit"
      value="Save"
      className="btn btn-default"
      onClick={props.saveAuthor} />

    <Prompt
      when={props.isChanged}
      message={location => `Leave and go to ${location.pathname}`} />
  </form>
)

AuthorForm.propTypes = {
  author: PropTypes.object.isRequired,
  updateAuthor: PropTypes.func.isRequired,
  saveAuthor: PropTypes.func.isRequired,
  errors: PropTypes.object
}

module.exports = AuthorForm
