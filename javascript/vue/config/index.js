export default {
  port: process.env.DEV_PORT || 8080,

  dev: {
    env: {
      NODE_ENV: JSON.stringify('development')
    }
  },

  prod: {
    env: {
      NODE_ENV: JSON.stringify('production')
    }
  },

  test: {
    env: {
      NODE_ENV: JSON.stringify('test')
    }
  }
}
