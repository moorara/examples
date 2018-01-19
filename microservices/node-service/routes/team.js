const express = require('express')
const bodyParser = require('body-parser')

const Logger = require('../util/logger')
const Middleware = require('../middleware')
const TeamService = require('../services/team')

class TeamRouter {
  constructor (config, options) {
    options = options || {}
    this.logger = options.logger || new Logger('TeamRouter')
    this.teamService = options.teamService || new TeamService(config, options)

    this.router = express.Router()
    this.router.use(bodyParser.json())

    this.router.post('*', Middleware.ensureJson())
    this.router.put('*', Middleware.ensureJson())

    this.router.route('/')
      .post(this.postTeam.bind(this))
      .get(this.getTeams.bind(this))

    this.router.route('/:id')
      .get(this.getTeam.bind(this))
      .put(this.putTeam.bind(this))
      .delete(this.deleteTeam.bind(this))
  }

  async postTeam (req, res, next) {
    let team
    let specs = req.body

    try {
      team = await this.teamService.create(specs)
      res.status(201).send(team)
    } catch (err) {
      this.logger.error('Failed to create new team.', err)
      return next(err)
    }
  }

  async getTeams (req, res, next) {
    let teams
    let query = req.query

    try {
      teams = await this.teamService.getAll(query)
      res.status(200).send(teams)
    } catch (err) {
      this.logger.error('Failed to get teams.', err)
      return next(err)
    }
  }

  async getTeam (req, res, next) {
    let team
    let id = req.params.id

    try {
      team = await this.teamService.get(id)
    } catch (err) {
      this.logger.error(`Failed to get team ${id}.`, err)
      return next(err)
    }

    if (!team) {
      res.sendStatus(404)
    } else {
      res.status(200).send(team)
    }
  }

  async putTeam (req, res, next) {
    let team
    let id = req.params.id
    let specs = req.body

    try {
      team = await this.teamService.update(id, specs)
    } catch (err) {
      this.logger.error(`Failed to update team ${id}.`, err)
      return next(err)
    }

    if (!team) {
      res.sendStatus(404)
    } else {
      res.status(200).send(team)
    }
  }

  async deleteTeam (req, res, next) {
    let team
    let id = req.params.id

    try {
      team = await this.teamService.delete(id)
    } catch (err) {
      this.logger.error(`Failed to delete team ${id}.`, err)
      return next(err)
    }

    if (!team) {
      res.sendStatus(404)
    } else {
      res.status(200).send(team)
    }
  }
}

module.exports = TeamRouter
