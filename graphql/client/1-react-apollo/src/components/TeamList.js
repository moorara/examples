import React from 'react'
import { gql } from 'apollo-boost'
import { graphql } from 'react-apollo'

import Team from './Team'

const teamsQuery = gql`
{
  teams {
    id
    name
    members {
      id
      name
      email
    }
  }
}
`

class TeamList extends React.Component {
  render () {
    console.log(this.props)
    const { data } = this.props

    if (data.loading) {
      return (<div>loading ....</div>)
    }

    return (
      <div>
        <h1>Teams:</h1>
        {data.teams.map(team =>
          <Team key={team.id} team={team} />
        )}
      </div>
    )
  }
}

export default graphql(teamsQuery)(TeamList)
