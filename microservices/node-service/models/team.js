const _ = require('lodash')
const mongoose = require('mongoose')

const MODEL_NAME = 'Team'
const PUBLIC_PROPERTIES = [ 'id', 'name', 'size' ]

class Team {
  constructor (config, options) {
    options = options || {}
    this.Model = this._model()
  }

  _model () {
    if (mongoose.models[MODEL_NAME]) {
      return mongoose.model(MODEL_NAME)
    }

    const schema = new mongoose.Schema({
      size: Number,
      name: { type: String, unique: true, required: true, index: true },
      createdAt: { type: Date, default: Date.now },
      updatedAt: { type: Date, default: Date.now }
    })

    schema.set('toJSON', {
      transform: doc => _.pick(doc, PUBLIC_PROPERTIES)
    })

    schema.pre('save', function (next) {
      this.updatedAt = Date.now()
      next()
    })

    schema.methods.formatName = function () {
      return this.name.replace(/\b\w/g, l => l.toUpperCase())
    }

    return mongoose.model(MODEL_NAME, schema)
  }
}

module.exports = Team
