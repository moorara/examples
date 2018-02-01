const React = require('react')
const Link = require('react-router-dom').Link

const Header = (props) => (
  <nav className="navbar navbar-default">
    <div className="container-fluid">
      <Link to="/" className="navbar-brand">
        <img src="images/logo.jpg" />
      </Link>
      <ul className="nav navbar-nav">
        <li><Link to="/">Home</Link></li>
        <li><Link to="/authors">Authors</Link></li>
        <li><Link to="/about">About</Link></li>
        <li><Link to="/about/us">Redirect</Link></li>
        <li><Link to="/not/found">404</Link></li>
      </ul>
    </div>
  </nav>
)

module.exports = Header
