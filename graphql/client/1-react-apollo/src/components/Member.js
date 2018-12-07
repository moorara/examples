import React from 'react'

class Member extends React.Component {
  render () {
    const { member } = this.props

    return (
      <li>{member.name}</li>
    )
  }
}

export default Member
