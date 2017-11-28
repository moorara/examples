import webpack from 'webpack'
import merge from 'webpack-merge'

import config from './config'
import webpackBaseConfig from './webpack.base.config'

const webpackConfig = merge(webpackBaseConfig, {
  devtool: 'inline-source-map',
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
      'process.env': config.test.env
    })
  ]
})

// Entry is not needed for unit testing
delete webpackConfig.entry

export default webpackConfig
