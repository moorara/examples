import React from 'react'
import { Link } from 'react-router-dom'

const HomePage = (props) => (
  <div className="jumbotron home">
    <h1>Administration App</h1>
    <p>React, Redux, and React Router in ES2017 for ultra-responsive web apps</p>
    <br />
    <Link to="about" className="btn btn-info">Learn More</Link>
  </div>
)

export default HomePage
