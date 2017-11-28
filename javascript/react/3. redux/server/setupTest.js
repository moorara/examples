const { JSDOM } = require('jsdom')
const babelRegister = require('babel-register')

const dom = new JSDOM('')
const exposedProperties = [ 'window', 'document', 'navigator' ]

babelRegister()
process.env.NODE_ENV = 'test'

// Disable webpack-specific features
require.extensions['.css'] = () => null
require.extensions['.png'] = () => null
require.extensions['.jpg'] = () => null

global.window = dom.window
global.document = dom.window.document
global.navigator = { userAgent: 'node.js' }

for (let property in window) {
  if (window.hasOwnProperty(property) && typeof global[property] === 'undefined') {
    exposedProperties.push(property)
    global[property] = window[property]
  }
}

documentRef = document // eslint-disable-line no-undef
