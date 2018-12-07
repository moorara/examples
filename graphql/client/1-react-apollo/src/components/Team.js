import React from 'react'

import Member from './Member'

class Team extends React.Component {
  render () {
    const { team } = this.props

    return (
      <div>
        <h2>{team.name}</h2>
        <ul>
          <li>ID: {team.id}</li>
          <ul>
            {team.members.map(member =>
              <Member key={member.id} member={member} />
            )}
          </ul>
        </ul>
      </div>
    )
  }
}

export default Team
