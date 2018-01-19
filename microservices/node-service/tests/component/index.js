/* eslint-env mocha */
require('should-http')
const should = require('should')
const rp = require('request-promise')

const serviceUrl = process.env.SERVICE_URL || 'http://localhost:4020'

describe('node-service', () => {
  let opts

  beforeEach(() => {
    opts = {
      json: true,
      simple: false,
      resolveWithFullResponse: true
    }
  })

  describe('general endpoints', () => {
    it('GET /health', done => {
      opts.method = 'GET'
      opts.uri = `${serviceUrl}/health`
      rp(opts).then(res => {
        res.should.have.status(200)
        res.body.should.equal('OK')
        done()
      }).catch(done)
    })

    it('GET /metrics', done => {
      opts.method = 'GET'
      opts.uri = `${serviceUrl}/metrics`
      rp(opts).then(res => {
        res.should.have.status(200)
        res.body.should.match(/# TYPE nodejs_version_info gauge/)
        res.body.should.match(/# TYPE http_requests_duration_seconds histogram/)
        res.body.should.match(/# TYPE http_requests_duration_quantiles_seconds summary/)
        done()
      }).catch(done)
    })
  })

  describe('team with name', () => {
    let id
    const name = 'cool'
    const newName = 'super cool'

    it('POST /teams', done => {
      opts.method = 'POST'
      opts.uri = `${serviceUrl}/teams`
      opts.body = { name }
      rp(opts).then(res => {
        id = res.body.id
        res.should.have.status(201)
        should.exist(res.body.id)
        res.body.name.should.equal(name)
        done()
      }).catch(done)
    })

    it('GET /teams', done => {
      opts.method = 'GET'
      opts.uri = `${serviceUrl}/teams`
      rp(opts).then(res => {
        res.should.have.status(200)
        res.body.should.have.length(1)
        res.body[0].id.should.equal(id)
        res.body[0].name.should.equal(name)
        done()
      }).catch(done)
    })

    it('GET /teams/:id', done => {
      opts.method = 'GET'
      opts.uri = `${serviceUrl}/teams/${id}`
      rp(opts).then(res => {
        res.should.have.status(200)
        res.body.id.should.equal(id)
        res.body.name.should.equal(name)
        done()
      }).catch(done)
    })

    it('PUT /teams/:id', done => {
      opts.method = 'PUT'
      opts.uri = `${serviceUrl}/teams/${id}`
      opts.body = { name: newName }
      rp(opts).then(res => {
        res.should.have.status(200)
        res.body.id.should.equal(id)
        res.body.name.should.equal(newName)
        done()
      }).catch(done)
    })

    it('DELETE /teams/:id', done => {
      opts.method = 'DELETE'
      opts.uri = `${serviceUrl}/teams/${id}`
      rp(opts).then(res => {
        res.should.have.status(200)
        res.body.id.should.equal(id)
        res.body.name.should.equal(newName)
        done()
      }).catch(done)
    })
  })

  describe('team with name and size', () => {
    let id
    const name = 'awesome'
    const size = 5
    const newName = 'super awesome'
    const newSize = 7

    it('POST /teams', done => {
      opts.method = 'POST'
      opts.uri = `${serviceUrl}/teams`
      opts.body = { name, size }
      rp(opts).then(res => {
        id = res.body.id
        res.should.have.status(201)
        should.exist(res.body.id)
        res.body.name.should.equal(name)
        res.body.size.should.equal(size)
        done()
      }).catch(done)
    })

    it('GET /teams', done => {
      opts.method = 'GET'
      opts.uri = `${serviceUrl}/teams`
      rp(opts).then(res => {
        res.should.have.status(200)
        res.body.should.have.length(1)
        res.body[0].id.should.equal(id)
        res.body[0].name.should.equal(name)
        res.body[0].size.should.equal(size)
        done()
      }).catch(done)
    })

    it('GET /teams/:id', done => {
      opts.method = 'GET'
      opts.uri = `${serviceUrl}/teams/${id}`
      rp(opts).then(res => {
        res.should.have.status(200)
        res.body.id.should.equal(id)
        res.body.name.should.equal(name)
        res.body.size.should.equal(size)
        done()
      }).catch(done)
    })

    it('PUT /teams/:id', done => {
      opts.method = 'PUT'
      opts.uri = `${serviceUrl}/teams/${id}`
      opts.body = { name: newName, size: newSize }
      rp(opts).then(res => {
        res.should.have.status(200)
        res.body.id.should.equal(id)
        res.body.name.should.equal(newName)
        res.body.size.should.equal(newSize)
        done()
      }).catch(done)
    })

    it('DELETE /teams/:id', done => {
      opts.method = 'DELETE'
      opts.uri = `${serviceUrl}/teams/${id}`
      rp(opts).then(res => {
        res.should.have.status(200)
        res.body.id.should.equal(id)
        res.body.name.should.equal(newName)
        res.body.size.should.equal(newSize)
        done()
      }).catch(done)
    })
  })
})
