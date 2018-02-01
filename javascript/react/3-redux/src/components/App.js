import React from 'react'
import PropTypes from 'prop-types'
import { connect } from 'react-redux'
import { BrowserRouter as Router } from 'react-router-dom'

import Routes from './Routes'
import Header from './common/Header'

const App = (props) => (
  <Router>
    <div>
      <Header isLoading={props.isLoading} />
      <Routes />
    </div>
  </Router>
)

App.propTypes = {
  isLoading: PropTypes.bool.isRequired
}

function mapStateToProps (state, ownProps) {
  return {
    isLoading: state.apiCallsInProgress > 0
  }
}

export default connect(mapStateToProps)(App)
