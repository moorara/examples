$ = jQuery = require('jquery')

const React = require('react')
const ReactDOM = require('react-dom')
const Router = require('react-router-dom').BrowserRouter

const Routes = require('./routes')
const Header = require('./components/common/header')
const AuthorActions = require('./actions/author')

const App = (props) => {
  return (
    <Router>
      <div>
        <Header />
        <Routes />
      </div>
    </Router>
  )
}

AuthorActions.getAuthors()
ReactDOM.render(<App />, document.getElementById('app'))
