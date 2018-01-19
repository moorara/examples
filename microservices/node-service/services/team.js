const _ = require('lodash')

const Logger = require('../util/logger')
const Team = require('../models/team')

class TeamService {
  constructor (config, options) {
    options = options || {}
    this.logger = options.logger || new Logger('TeamService')
    this.TeamModel = options.TeamModel || new Team().Model
  }

  async create (specs) {
    const team = new this.TeamModel(
      _.pick(specs, [ 'name', 'size' ])
    )

    try {
      let savedTeam = await team.save()
      return savedTeam.toJSON()
    } catch (err) {
      this.logger.error(`Error creating team ${specs.name}.`, err)
      throw err
    }
  }

  /**
   * query: name, minSize, maxSize, limit, skip
   */
  async getAll (query) {
    let teams
    let mongoQuery = {}

    query = query || {}
    if (query.name) mongoQuery.name = new RegExp(`.*${query.name}.*`)
    if (query.minSize) _.set(mongoQuery, 'size.$gte', +query.minSize)
    if (query.maxSize) _.set(mongoQuery, 'size.$lte', +query.maxSize)

    try {
      teams = await this.TeamModel
        .find(mongoQuery)
        .limit(+query.limit)
        .skip(+query.skip)
        .exec()
    } catch (err) {
      this.logger.error('Error getting teams.', err)
      throw err
    }

    return teams.map(t => t.toJSON())
  }

  async get (id) {
    try {
      let team = await this.TeamModel.findById(id)
      return team ? team.toJSON() : null
    } catch (err) {
      this.logger.error(`Error getting team ${id}.`, err)
      throw err
    }
  }

  async update (id, specs) {
    let query = {}
    query = _.merge(query, _.pick(specs, [ 'name', 'size' ]))

    try {
      let team = await this.TeamModel.findByIdAndUpdate(id, query, { new: true })
      return team ? team.toJSON() : null
    } catch (err) {
      this.logger.error(`Error updating team ${id}.`, err)
      throw err
    }
  }

  async delete (id) {
    try {
      let team = await this.TeamModel.findByIdAndRemove(id)
      return team ? team.toJSON() : null
    } catch (err) {
      this.logger.error(`Error deleting team ${id}.`, err)
      throw err
    }
  }
}

module.exports = TeamService
