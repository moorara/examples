import React, { Component } from 'react'
import ApolloClient from 'apollo-boost'
import { ApolloProvider } from 'react-apollo'

import TeamList from './components/TeamList'

const client = new ApolloClient({
  uri: 'http://localhost:4000'
})

class App extends Component {
  render() {
    return (
      <ApolloProvider client={client}>
        <div className="App">
          <TeamList />
        </div>
      </ApolloProvider>
    );
  }
}

export default App;
