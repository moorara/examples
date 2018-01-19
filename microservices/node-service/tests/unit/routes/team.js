/* eslint-env mocha */
const express = require('express')
const sinon = require('sinon')
const should = require('should')
const supertest = require('supertest')

const TeamRouter = require('../../../routes/team')

describe('TeamRouter', () => {
  let config, logger
  let teamService, _teamService
  let router, _router
  let app, request

  beforeEach(() => {
    config = {}
    logger = {
      debug () {},
      verbose () {},
      info () {},
      warn () {},
      error () {},
      fatal () {}
    }

    teamService = {
      create () {},
      getAll () {},
      get () {},
      update () {},
      delete () {}
    }
    _teamService = sinon.mock(teamService)

    router = new TeamRouter(config, {
      logger,
      teamService
    })
    _router = sinon.mock(router)

    app = express()
    app.use('/', router.router)
    request = supertest(app)
  })

  afterEach(() => {
    _teamService.restore()
    _router.restore()
  })

  describe('constructor', () => {
    it('should create a new router with defaults', () => {
      router = new TeamRouter({})
      should.exist(router.router)
      should.exist(router.logger)
      should.exist(router.teamService)
    })
  })

  describe('POST /teams', () => {
    let specs, team

    beforeEach(() => {
      specs = { name: 'awesome', size: 5 }
      team = { id: '1111-aaaa', name: 'awesome', size: 5 }
    })

    it('201 - create a team', done => {
      _teamService.expects('create').withArgs(specs).resolves(team)
      request.post('/')
        .send(specs)
        .expect(201, team, done)
    })
    it('415 - error when body is not json', done => {
      request.post('/')
        .send('invalid json')
        .expect(415, done)
    })
    it('500 - error when team service fails', done => {
      _teamService.expects('create').withArgs(specs).rejects(new Error('error'))
      request.post('/')
        .send(specs)
        .expect(500, done)
    })
  })

  describe('GET /teams', () => {
    let query, teams

    beforeEach(() => {
      query = { name: 'we', minSize: '3', maxSize: '7', limit: '10', skip: '5' }
      teams = [
        { id: '1111-aaaa', name: 'awesome', size: 5 },
        { id: '2222-bbbb', name: 'nice', size: 6 },
        { id: '3333-cccc', name: 'great', size: 7 }
      ]
    })

    it('200 - get teams', done => {
      _teamService.expects('getAll').withArgs(query).resolves(teams)
      request.get('/?name=we&minSize=3&maxSize=7&limit=10&skip=5')
        .expect(200, teams, done)
    })
    it('500 - error when team service fails', done => {
      _teamService.expects('getAll').withArgs(query).rejects(new Error('error'))
      request.get('/?name=we&minSize=3&maxSize=7&limit=10&skip=5')
        .expect(500, done)
    })
  })

  describe('GET /teams/:id', () => {
    let id, team

    beforeEach(() => {
      id = '1111-aaaa'
      team = { id, name: 'awesome', size: 5 }
    })

    it('200 - get a team', done => {
      _teamService.expects('get').withArgs(id).resolves(team)
      request.get(`/${id}`)
        .expect(200, team, done)
    })
    it('404 - team not found', done => {
      _teamService.expects('get').withArgs(id).resolves()
      request.get(`/${id}`)
        .expect(404, done)
    })
    it('500 - error when team service fails', done => {
      _teamService.expects('get').withArgs(id).rejects(new Error('error'))
      request.get(`/${id}`)
        .expect(500, done)
    })
  })

  describe('PUT /teams/:id', () => {
    let id, specs, team

    beforeEach(() => {
      id = '2222-bbbb'
      specs = { name: 'cool', size: 3 }
      team = { id, name: 'cool', size: 3 }
    })

    it('200 - update a team', done => {
      _teamService.expects('update').withArgs(id, specs).resolves(team)
      request.put(`/${id}`)
        .send(specs)
        .expect(200, team, done)
    })
    it('404 - team not found', done => {
      _teamService.expects('update').withArgs(id, specs).resolves()
      request.put(`/${id}`)
        .send(specs)
        .expect(404, done)
    })
    it('415 - error when body is not json', done => {
      request.put(`/${id}`)
        .send('invalid json')
        .expect(415, done)
    })
    it('500 - error when team service fails', done => {
      _teamService.expects('update').withArgs(id, specs).rejects(new Error('error'))
      request.put(`/${id}`)
        .send(specs)
        .expect(500, done)
    })
  })

  describe('DELETE /teams/:id', () => {
    let id, team

    beforeEach(() => {
      id = '3333-cccc'
      team = { id, name: 'great', size: 7 }
    })

    it('200 - delete a team', done => {
      _teamService.expects('delete').withArgs(id).resolves(team)
      request.delete(`/${id}`)
        .expect(200, team, done)
    })
    it('404 - team not found', done => {
      _teamService.expects('delete').withArgs(id).resolves()
      request.delete(`/${id}`)
        .expect(404, done)
    })
    it('500 - error when team service fails', done => {
      _teamService.expects('delete').withArgs(id).rejects(new Error('error'))
      request.delete(`/${id}`)
        .expect(500, done)
    })
  })
})
