import React from 'react'
import PropTypes from 'prop-types'
import debounce from 'lodash.debounce'

import storeProvider from './storeProvider'

class SearchBar extends React.PureComponent {
  static propTypes = {
    store: PropTypes.object.isRequired
  }

  state = {
    searchTerm: ''
  }

  doSearch = debounce(() => {
    this.props.store.setSearchTerm(this.state.searchTerm)
  }, 300)

  handleSearch = (event) => {
    this.setState({ searchTerm: event.target.value }, () => {
      this.doSearch()
    })
  }

  render () {
    return (
      <input
        type='search'
        placeholder='Enter search term'
        value={this.state.searchTerm}
        onChange={this.handleSearch}
      />
    )
  }
}

export default storeProvider()(SearchBar)
