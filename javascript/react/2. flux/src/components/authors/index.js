const React = require('react')
const Link = require('react-router-dom').Link

const AuthorList = require('./authorList')
const AuthorStore = require('../../stores/author')

class Authors extends React.Component {
  constructor (props) {
    super(props)
    this.state = {
      authors: AuthorStore.getAuthors()
    }
  }

  _onChange () {
    this.setState({
      authors: AuthorStore.getAuthors()
    })
  }

  componentWillMount () {
    AuthorStore.addChangeListener(this._onChange.bind(this))
  }

  componentWillUnmount () {
    AuthorStore.removeChangeListener(this._onChange.bind(this))
  }

  render () {
    const { authors } = this.state
    return (
      <div style={{ margin: 40 }}>
        <h1>Authors</h1>
        <br />
        <AuthorList authors={authors} />
        <br />
        <Link to="/author" className="btn btn-primary" >Add Author</Link>
      </div>
    )
  }
}

module.exports = Authors
