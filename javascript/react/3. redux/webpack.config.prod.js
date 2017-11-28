import path from 'path'
import webpack from 'webpack'
import UglifyJSPlugin from 'uglifyjs-webpack-plugin'
import ExtractTextPlugin from 'extract-text-webpack-plugin'

const GLOBALS = {
  'process.env': {
    'NODE_ENV': JSON.stringify('production')
  }
}

const config = {
  target: 'web',
  devtool: 'source-map',
  entry: path.resolve(__dirname, 'src/index.js'),
  output: {
    path: path.resolve(__dirname, 'dist'),
    publicPath: '/',
    filename: 'bundle.js'
  },
  plugins: [
    new webpack.DefinePlugin(GLOBALS),
    new ExtractTextPlugin('styles.css'),
    new UglifyJSPlugin()
  ],
  module: {
    rules: [
      // JavaScript
      { test: /\.js$/, include: path.join(__dirname, 'src'), use: [ 'babel-loader' ] },
      { test: /\.jsx$/, include: path.join(__dirname, 'src'), use: [ 'babel-loader' ] },

      // Stylesheets
      { test: /\.css$/, use: ExtractTextPlugin.extract({ use: 'css-loader', fallback: 'style-loader' }) },

      // Fonts
      { test: /\.eot(\?v=\d+.\d+.\d+)?$/, use: 'file-loader'},
      { test: /\.ttf(\?v=\d+\.\d+\.\d+)?$/, use: 'url-loader?limit=10000&mimetype=application/octet-stream' },
      { test: /\.woff(2)?(\?v=[0-9]\.[0-9]\.[0-9])?$/, use: 'url-loader?limit=10000&mimetype=application/font-woff' },
      { test: /\.svg(\?v=\d+\.\d+\.\d+)?$/, use: 'url-loader?limit=10000&mimetype=image/svg+xml' }
    ]
  }
}

export default config
