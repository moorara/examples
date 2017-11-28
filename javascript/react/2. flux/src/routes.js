const React = require('react')
const Route = require('react-router-dom').Route
const Switch = require('react-router-dom').Switch
const Redirect = require('react-router-dom').Redirect

const Home = require('./components/home')
const About = require('./components/about')
const Authros = require('./components/authors')
const ManageAuthor = require('./components/authors/manageAuthor')
const NotFound = require('./components/notFound')

const Routes = (props) => (
  <div>
    <Switch>
      <Route exact path="/" component={Home} />
      <Route exact path="/authors" component={Authros} />
      <Route exact path="/author" component={ManageAuthor} />
      <Route exact path="/authors/:id" component={ManageAuthor} />
      <Route exact path="/about" component={About} />
      <Redirect from="/about/*" to="/about" />
      <Route component={NotFound} />
    </Switch>
  </div>
)

module.exports = Routes
