const _ = require('lodash')
const React = require('react')
const toastr = require('toastr')

const AuthorForm = require('./authorForm')
const AuthorStore = require('../../stores/author')
const AuthorActions = require('../../actions/author')

class ManageAuthor extends React.Component {
  constructor (props) {
    super(props)
    this.state = {
      author: { id: '', firstname: '', lastname: '' },
      isChanged: false,
      errors: {}
    }
  }

  componentWillMount () {
    let authorId = this.props.match.params.id
    if (authorId) {
      this.setState({
        author: AuthorStore.getAuthor(authorId)
      })
    }
  }

  validateForm () {
    let errors = {}
    if (this.state.author.firstname.length < 3) errors.firstname = "Invalid first name"
    if (this.state.author.lastname.length < 3) errors.lastname = "Invalid last name"
    this.setState({ errors })
    return _.isEmpty(errors)
  }

  updateAuthor (event) {
    let prop = event.target.name
    let value = event.target.value
    this.state.author[prop] = value
    return this.setState({
      author: this.state.author,
      isChanged: true
    })
  }

  saveAuthor (event) {
    event.preventDefault()
    if (this.validateForm()) {
      let action = this.state.author.id ? AuthorActions.updateAuthor : AuthorActions.createAuthor
      action(this.state.author)
      this.setState({ isChanged: false })
      toastr.success('Author Saved.')
      this.props.history.push('/authors')
    }
  }

  render () {
    return (
      <div style={{ margin: 40 }}>
        <h1>Add Author</h1>
        <br />

        <AuthorForm
          author={this.state.author}
          isChanged={this.state.isChanged}
          errors={this.state.errors}
          updateAuthor={this.updateAuthor.bind(this)}
          saveAuthor={this.saveAuthor.bind(this)} />
      </div>
    )
  }
}

module.exports = ManageAuthor
