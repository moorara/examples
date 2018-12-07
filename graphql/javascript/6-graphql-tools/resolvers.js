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
  Query: {
    teams (_, args, context, info) {
      return Object.assign(store.teams)
    },
    members (_, { teamId }, context, info) {
      return store.members.filter(m => m.teamId === teamId)
    }
  },

  Mutation: {
    addTeam (_, { name }, context, info) {
      const newTeam = { id: 'cccc', name }
      store.teams.push(newTeam)
      return newTeam
    },
    addMember (_, { teamId, name, email }, context, info) {
      const newTeam = { id: '5555', teamId, name, email }
      store.members.push(newTeam)
      return newTeam
    }
  },

  Team: {
    id: t => t.id,
    name: t => t.name,
    members: t => store.members.filter(m => m.teamId === t.id)
  },

  Member: {
    id: m => m.id,
    teamId: m => m.teamId,
    name: m => m.name,
    email: m => m.email,
    team: m => store.teams.find(t => t.id === m.teamId)
  }
}
