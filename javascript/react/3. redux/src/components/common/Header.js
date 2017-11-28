import React from 'react'
import PropTypes from 'prop-types'
import { Link } from 'react-router-dom'

import LoadingDots from './LoadingDots'

const Header = ({ isLoading }) => (
  <nav className="navbar navbar-default">
    <div className="container-fluid">
      <ul className="nav navbar-nav">
        <li><Link to="/">Home</Link></li>
        <li><Link to="/courses">Courses</Link></li>
        <li><Link to="/about">About</Link></li>
      </ul>
      { isLoading && <LoadingDots dots={10} interval={100} /> }
    </div>
  </nav>
)

Header.propTypes = {
  isLoading: PropTypes.bool.isRequired
}

export default Header
