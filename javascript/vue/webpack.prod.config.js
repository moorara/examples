import path from 'path'
import webpack from 'webpack'
import merge from 'webpack-merge'
import HtmlWebpackPlugin from 'html-webpack-plugin'
import UglifyJSPlugin from 'uglifyjs-webpack-plugin'
import ExtractTextPlugin from 'extract-text-webpack-plugin'

import config from './config'
import webpackBaseConfig from './webpack.base.config'

export default merge(webpackBaseConfig, {
  devtool: 'source-map',
  module: {
    rules: [
      // Stylesheets
      {
        test: /\.css$/,
        use: ExtractTextPlugin.extract({
          fallback: 'style-loader',
          use: {
            loader: 'css-loader',
            options: {
              minimize: true,
              sourceMap: true
            }
          }
        })
      },
      {
        test: /\.scss|sass$/,
        use: ExtractTextPlugin.extract({
          fallback: 'style-loader',
          use: [ 'css-loader', 'sass-loader' ]
        })
      },
      {
        test: /\.less$/,
        use: ExtractTextPlugin.extract({
          fallback: 'style-loader',
          use: [ 'css-loader', 'less-loader' ]
        })
      }
    ]
  },
  plugins: [
    new webpack.DefinePlugin({
      'process.env': config.prod.env
    }),
    // https://github.com/webpack-contrib/uglifyjs-webpack-plugin
    new UglifyJSPlugin({
      sourceMap: true
    }),
    // https://github.com/webpack-contrib/extract-text-webpack-plugin
    new ExtractTextPlugin({
      filename: '[name].[contenthash].css'
    }),
    // https://github.com/jantimon/html-webpack-plugin
    new HtmlWebpackPlugin({
      template: path.resolve(__dirname, 'src/index.html'),
      filename: path.resolve(__dirname, 'dist/index.html'),
      inject: true,
      minify: {
        html5: true,
        removeComments: true,
        collapseWhitespace: true,
        removeAttributeQuotes: true
        // https://github.com/kangax/html-minifier#options-quick-reference
      },
      chunksSortMode: 'dependency'
    })
  ]
})
