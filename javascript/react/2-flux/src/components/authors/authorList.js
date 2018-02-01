const React = require('react')
const PropTypes = require('prop-types')
const Link = require('react-router-dom').Link
const toastr = require('toastr')

const AuthorActions = require('../../actions/author')

class AuthorList extends React.Component {
  deleteAuthor (id, event) {
    event.preventDefault()
    AuthorActions.deleteAuthor(id)
    toastr.success('Author Deleted.')
  }

  render () {
    return (
      <table className="table table-striped">
        <caption>{this.props.caption}</caption>
        <thead>
          <tr>
            <th>Firstname</th>
            <th>Lastname</th>
            <th>ID</th>
            <th></th>
          </tr>
        </thead>
        <tbody>
          {this.props.authors.map(author => (
            <tr key={author.id}>
              <td>{author.firstname}</td>
              <td>{author.lastname}</td>
              <td><Link to={`/authors/${author.id}`}>{author.id}</Link></td>
              <td><a href="#" onClick={this.deleteAuthor.bind(this, author.id)}>Delete</a></td>
            </tr>
          ))}
        </tbody>
      </table>
    )
  }
}

AuthorList.propTypes = {
  authors: PropTypes.array.isRequired
}

AuthorList.defaultProps = {
  caption: 'All Results'
}

module.exports = AuthorList
