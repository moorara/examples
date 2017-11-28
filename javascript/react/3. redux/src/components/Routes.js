import React from 'react'
import { Route, Switch, Redirect } from 'react-router-dom'

import HomePage from './home/HomePage'
import CoursesPage from './course/CoursesPage'
import ManageCoursePage from './course/ManageCoursePage'
import AboutPage from './about/AboutPage'

const Routes = (props) => (
  <Switch>
    <Route exact path="/" component={HomePage} />
    <Route exact path="/courses" component={CoursesPage} />
    <Route exact path="/course" component={ManageCoursePage} />
    <Route exact path="/course/:id" component={ManageCoursePage} />
    <Route exact path="/about" component={AboutPage} />
    <Redirect from="/about-us" to="/about" />
  </Switch>
)

export default Routes
