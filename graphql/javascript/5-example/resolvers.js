const store = {
  teams: [
    { id: 'aaaa', name: 'back-end', members: [] },
    { id: 'bbbb', name: 'front-end', members: [] }
  ],
  members: [
    { id: '1111', teamId: 'aaaa', name: 'Arman', email: 'arman@example.com' },
    { id: '2222', teamId: 'aaaa', name: 'Milad' },
    { id: '3333', teamId: 'bbbb', name: 'Ankur', email: 'ankur@example.com' },
    { id: '4444', teamId: 'bbbb', name: 'John' }
  ]
}

module.exports = {
  // this!

  teams (args, context, info) {
    const teams = Object.assign(store.teams)
    for (let t of teams) {
      t.members = this.members(
        Object.assign(args, { teamId: t.id }),
        context,
        info
      )
    }
    return teams
  },

  members ({ teamId }, context, info) {
    return store.members.filter(m => m.teamId === teamId)
  },

  addTeam ({ name }, context, info) {
    const newTeam = { id: 'cccc', name }
    store.teams.push(newTeam)
    return newTeam
  },

  addMember ({ teamId, name, email }, context, info) {
    const newTeam = { id: '5555', teamId, name, email }
    store.members.push(newTeam)
    return newTeam
  }
}
