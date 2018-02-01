const path = require('path')
const webpack = require('webpack')

const config = {
  target: 'web',
  entry: {
    vendor: [
      'react',
      'react-dom',
      'prop-types',
      'axios',
      'lodash.debounce',
      'lodash.pickby'
    ],
    app: [ path.resolve(__dirname, 'src/renderers/dom.js') ]
  },
  output: {
    path: path.resolve(__dirname, 'public'),
    filename: '[name].js'
  },
  plugins: [
    new webpack.NoEmitOnErrorsPlugin(),
    new webpack.optimize.CommonsChunkPlugin({
      name: 'vendor'
    })
  ],
  module: {
    rules: [
      // JavaScript
      {
        test: /\.(js|jsx)$/,
        exclude: path.join(__dirname, 'node_modules'),
        use: {
          loader: 'babel-loader',
          options: {
            presets: ['react', 'env', 'stage-2']
          }
        }
      },

      // Stylesheets
      {
        test: /\.css$/,
        use: [ 'style-loader', 'css-loader' ]
      },

      // Fonts
      { test: /\.eot(\?v=\d+.\d+.\d+)?$/, use: 'file-loader' },
      { test: /\.ttf(\?v=\d+\.\d+\.\d+)?$/, use: 'url-loader?limit=10000&mimetype=application/octet-stream' },
      { test: /\.woff(2)?(\?v=[0-9]\.[0-9]\.[0-9])?$/, use: 'url-loader?limit=10000&mimetype=application/font-woff' },
      { test: /\.svg(\?v=\d+\.\d+\.\d+)?$/, use: 'url-loader?limit=10000&mimetype=image/svg+xml' }
    ]
  }
}

module.exports = config
