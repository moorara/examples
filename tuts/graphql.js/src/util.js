const { formatError } = require('graphql')

module.exports = {
  formatError (err) {
    const data = formatError(err)
    const { originalError } = err
    data.field = originalError && originalError.field
    return data
  }
}
