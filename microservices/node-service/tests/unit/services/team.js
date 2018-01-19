/* eslint-env mocha */
const sinon = require('sinon')
const should = require('should')

const TeamService = require('../../../services/team')

describe('TeamService', () => {
  let config, logger
  let Model, _Model
  let modelInstance, _modelInstance
  let service, _service

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

    Model = function (data) {
      return modelInstance
    }
    Model.limit = () => {}
    Model.skip = () => {}
    Model.exec = () => {}
    Model.find = () => {}
    Model.findById = () => {}
    Model.findByIdAndUpdate = () => {}
    Model.findByIdAndRemove = () => {}
    _Model = sinon.mock(Model)

    modelInstance = {}
    modelInstance.save = () => {}
    _modelInstance = sinon.mock(modelInstance)

    service = new TeamService(config, {
      logger,
      TeamModel: Model
    })
    _service = sinon.mock(service)
  })

  afterEach(() => {
    _Model.restore()
    _modelInstance.restore()
    _service.restore()
  })

  describe('constructor', () => {
    it('should create a new service with defaults', () => {
      service = new TeamService({})
      should.exist(service.logger)
      should.exist(service.TeamModel)
    })
  })

  describe('create', () => {
    let specs
    let savedTeam

    beforeEach(() => {
      specs = { name: 'Great', size: '5' }
      savedTeam = Object.assign({}, specs, {
        id: '2222-bbbb-4444-dddd',
        createdAt: new Date(),
        updatedAt: new Date()
      })
      savedTeam.toJSON = () => savedTeam
    })

    it('should reject with error when model query fails', done => {
      _modelInstance.expects('save').rejects(new Error('error'))
      service.create(specs).catch(err => {
        _modelInstance.verify()
        should.exist(err)
        err.message.should.equal('error')
        done()
      })
    })
    it('should resolve with new team when model query succeeds', done => {
      _modelInstance.expects('save').resolves(savedTeam)
      service.create(specs).then(t => {
        _modelInstance.verify()
        t.should.eql(savedTeam)
        done()
      }).catch(done)
    })
  })

  describe('getAll', () => {
    let query
    let t1, t2, t3

    beforeEach(() => {
      query = {}

      t1 = { id: '1111-aaaa' }
      t1.toJSON = () => t1

      t2 = { id: '2222-bbbb' }
      t2.toJSON = () => t2

      t3 = { id: '3333-cccc' }
      t3.toJSON = () => t3
    })

    it('should rejects with error when model query fails', done => {
      _Model.expects('find').returns(Model)
      _Model.expects('limit').returns(Model)
      _Model.expects('skip').returns(Model)
      _Model.expects('exec').rejects(new Error('error'))
      service.getAll().catch(err => {
        _Model.verify()
        should.exist(err)
        err.message.should.equal('error')
        done()
      })
    })
    it('should resolves with retrieved teams when model query succeeds', done => {
      _Model.expects('find').returns(Model)
      _Model.expects('limit').returns(Model)
      _Model.expects('skip').returns(Model)
      _Model.expects('exec').resolves([ t1, t2, t3 ])
      service.getAll().then(teams => {
        _Model.verify()
        teams.should.eql([ t1, t2, t3 ])
        done()
      }).catch(done)
    })
    it('should resolves with retrieved teams when model query succeeds', done => {
      query = { name: 'fc', minSize: 3, maxSize: 10, limit: 20, skip: 10 }
      let mongoQuery = { name: /.*fc.*/, size: { $gte: 3, $lte: 10 } }
      _Model.expects('find').withArgs(mongoQuery).returns(Model)
      _Model.expects('limit').withArgs(+query.limit).returns(Model)
      _Model.expects('skip').withArgs(+query.skip).returns(Model)
      _Model.expects('exec').resolves([ t1, t2, t3 ])
      service.getAll(query).then(teams => {
        _Model.verify()
        teams.should.eql([ t1, t2, t3 ])
        done()
      }).catch(done)
    })
  })

  describe('get', () => {
    let id, team

    beforeEach(() => {
      id = '1111-aaaa'
      team = { id }
      team.toJSON = () => team
    })

    it('should rejects with error when model query fails', done => {
      _Model.expects('findById').withArgs(id).rejects(new Error('error'))
      service.get(id).catch(err => {
        _Model.verify()
        should.exist(err)
        err.message.should.equal('error')
        done()
      })
    })
    it('should resolves with empty result when model query returns no result', done => {
      _Model.expects('findById').withArgs(id).resolves()
      service.get(id).then(t => {
        _Model.verify()
        should.not.exist(t)
        done()
      }).catch(done)
    })
    it('should resolves with retrieved team when model query succeeds', done => {
      _Model.expects('findById').withArgs(id).resolves(team)
      service.get(id).then(t => {
        _Model.verify()
        t.id.should.equal(id)
        done()
      }).catch(done)
    })
  })

  describe('update', () => {
    let id, specs, team

    beforeEach(() => {
      id = '2222-bbbb'
      specs = { name: 'Awesome', size: 7 }
      team = { id, name: specs.name, size: specs.size }
      team.toJSON = () => team
    })

    it('should rejects with error when model query fails', done => {
      _Model.expects('findByIdAndUpdate').withArgs(id, specs).rejects(new Error('error'))
      service.update(id, specs).catch(err => {
        _Model.verify()
        should.exist(err)
        err.message.should.equal('error')
        done()
      })
    })
    it('should resolves with empty result when model query returns no result', done => {
      _Model.expects('findByIdAndUpdate').withArgs(id, specs).resolves()
      service.update(id, specs).then(t => {
        _Model.verify()
        should.not.exist(t)
        done()
      }).catch(done)
    })
    it('should resolves with updated team when model query succeeds', done => {
      _Model.expects('findByIdAndUpdate').withArgs(id, specs).resolves(team)
      service.update(id, specs).then(t => {
        _Model.verify()
        t.id.should.equal(id)
        done()
      }).catch(done)
    })
  })

  describe('delete', () => {
    let id, team

    beforeEach(() => {
      id = '3333-cccc'
      team = { id }
      team.toJSON = () => team
    })

    it('should rejects with error when model query fails', done => {
      _Model.expects('findByIdAndRemove').withArgs(id).rejects(new Error('error'))
      service.delete(id).catch(err => {
        _Model.verify()
        should.exist(err)
        err.message.should.equal('error')
        done()
      })
    })
    it('should resolves with empty result when model query returns no result', done => {
      _Model.expects('findByIdAndRemove').withArgs(id).resolves()
      service.delete(id).then(t => {
        _Model.verify()
        should.not.exist(t)
        done()
      }).catch(done)
    })
    it('should resolves with deleted team when model query succeeds', done => {
      _Model.expects('findByIdAndRemove').withArgs(id).resolves(team)
      service.delete(id).then(t => {
        _Model.verify()
        t.id.should.equal(id)
        done()
      }).catch(done)
    })
  })
})
