module.exports = {
  parser: 'babel-eslint',
  parserOptions: {
    ecmaFeatures: {
    jsx: true,
      experimentalObjectRestSpread: true,
    },
    sourceType: 'module',
  },
  env: {
    browser: true,
    commonjs: true,
    es6: true,
    node: true,
    jest: true,
  },
  plugins: ['react'],
  extends: ['eslint:recommended', 'standard', 'standard-react'],
  rules: {
    'comma-dangle': ['off'],
    'no-console': ['warn', { allow: ['info', 'error'] }],

    'react/prop-types': ['warn'],
  }
}
