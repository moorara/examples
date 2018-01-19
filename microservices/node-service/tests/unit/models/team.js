/* eslint-env mocha */
const should = require('should')

const Team = require('../../../models/team')

describe('Team', () => {
  let TeamModel

  beforeEach(() => {
    TeamModel = new Team().Model
  })

  describe('Model', () => {
    it('should create a new team with name', () => {
      let team = new TeamModel({
        name: 'Awesome'
      })

      should.exist(team.id)
      team.name.should.equal('Awesome')
      should.exist(team.createdAt)
      should.exist(team.updatedAt)
    })

    it('should create a new team with name and size', () => {
      let team = new TeamModel({
        name: 'warriors',
        size: 10
      })

      should.exist(team.id)
      team.name.should.equal('warriors')
      team.size.should.equal(10)
      should.exist(team.createdAt)
      should.exist(team.updatedAt)
    })

    it('should create a new team and format name with title case', () => {
      let team = new TeamModel({
        name: 'real madrid'
      })

      should.exist(team.id)
      team.name.should.equal('real madrid')
      should.exist(team.createdAt)
      should.exist(team.updatedAt)
      should.equal(team.formatName(), 'Real Madrid')
    })
  })

  describe('toJSON', () => {
    it('should return only public properties of new team', () => {
      let team = new TeamModel({
        name: 'Liverpool',
        size: 11
      }).toJSON()

      should.exist(team.id)
      team.name.should.equal('Liverpool')
      team.size.should.equal(11)
      should.not.exist(team.createdAt)
      should.not.exist(team.updatedAt)
    })
  })
})
