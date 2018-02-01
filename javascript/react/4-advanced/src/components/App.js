import React from 'react'
import PropTypes from 'prop-types'
import pickBy from 'lodash.pickby'

import Timestamp from './Timestamp'
import SearchBar from './SearchBar'
import ArticleList from './ArticleList'

class App extends React.PureComponent {
  static propTypes = {
    store: PropTypes.object.isRequired
  }

  static childContextTypes = {
    store: PropTypes.object.isRequired,
  }

  appState = () => {
    const { articles, searchTerm } = this.props.store.getState()
    return { articles, searchTerm }
  }

  state = this.appState();

  getChildContext () {
    return {
      store: this.props.store
    }
  }

  onStoreChange = () => {
    this.setState(this.appState)
  }

  componentDidMount () {
    this.props.store.startClock()
    this.subscriptionId = this.props.store.subscribe(this.onStoreChange)
  }

  componentWillUnmount () {
    this.props.store.unsubscribe(this.subscriptionId)
  }

  render () {
    let { articles, searchTerm } = this.state
    const searchRE = new RegExp(searchTerm, 'i')

    if (searchTerm) {
      articles = pickBy(articles, value => value.title.match(searchRE) || value.body.match(searchRE))
    }

    return (
      <div>
        <Timestamp />
        <SearchBar />
        <ArticleList
          articles={articles}
        />
      </div>
    )
  }
}

export default App
