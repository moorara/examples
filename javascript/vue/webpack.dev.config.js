import path from 'path'
import webpack from 'webpack'
import merge from 'webpack-merge'
import HtmlWebpackPlugin from 'html-webpack-plugin'
import FriendlyErrorsPlugin from 'friendly-errors-webpack-plugin'

import config from './config'
import webpackBaseConfig from './webpack.base.config'

// Add hot reload client to dev bundle
Object.keys(webpackBaseConfig.entry).forEach(name => {
  webpackBaseConfig.entry[name] = [ 'webpack-hot-middleware/client?reload=true' ].concat(webpackBaseConfig.entry[name])
})

export default merge(webpackBaseConfig, {
  devtool: 'eval-source-map',
  module: {
    rules: [
      // Stylesheets
      { test: /\.css$/, use: [ 'style-loader', 'css-loader' ] },
      { test: /\.less$/, use: [ 'style-loader', 'css-loader', 'less-loader' ] },
      { test: /\.scss|sass$/, use: [ 'style-loader', 'css-loader', 'sass-loader' ] }
    ]
  },
  plugins: [
    new webpack.DefinePlugin({
      'process.env': config.dev.env
    }),
    new webpack.HotModuleReplacementPlugin(),
    new webpack.NoEmitOnErrorsPlugin(),
    // https://github.com/jantimon/html-webpack-plugin
    new HtmlWebpackPlugin({
      template: path.resolve(__dirname, 'src/index.html'),
      filename: 'index.html',
      inject: true
    }),
    // https://github.com/geowarin/friendly-errors-webpack-plugin
    new FriendlyErrorsPlugin()
  ]
})
