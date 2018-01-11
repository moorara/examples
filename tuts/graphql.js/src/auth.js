const HEADER_REGEX = /bearer token-(.*)$/

module.exports.authenticate = async (req, Users) => {
  const authorization = req.headers.authorization
  const email = authorization && HEADER_REGEX.exec(authorization)[1]
  return email && await Users.findOne({ email })
}
