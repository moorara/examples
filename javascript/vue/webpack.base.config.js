import path from 'path'

export default {
  target: 'web',
  entry: {
    app: path.resolve(__dirname, 'src/main.js')
  },
  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: '[name].bundle.js',
    publicPath: '/'
  },
  resolve: {
    extensions: [ '.js', '.json', '.vue' ],
    alias: {
      'vue$': 'vue/dist/vue.esm.js'
    }
  },
  module: {
    rules: [
      // JavaScript
      {
        test: /\.js$/,
        use: [ 'babel-loader' ],
        include: [
          path.join(__dirname, 'src'),
          path.join(__dirname, 'test')
        ]
      },

      // Vue: https://vue-loader.vuejs.org
      {
        test: /\.vue$/,
        include: path.join(__dirname, 'src'),
        use: [{
          loader: 'vue-loader',
          options: {
            loaders: {
              css: 'vue-style-loader!css-loader',
              scss: 'vue-style-loader!css-loader!sass-loader',
              sass: 'vue-style-loader!css-loader!sass-loader',
              less: 'vue-style-loader!css-loader!less-loader'
            }
          }
        }]
      },

      // Fonts
      {
        test: /\.(woff2?|eot|ttf|otf)(\?.*)?$/,
        use: [{
          loader: 'url-loader',
          options: {
            limit: 10000,
            name: 'fonts/[name].[hash:7].[ext]' // option for file-loader fallback
          }
        }]
      },

      // Images
      {
        test: /\.(png|jpe?g|gif|svg)(\?.*)?$/,
        use: [{
          loader: 'url-loader',
          options: {
            limit: 10000,
            name: 'images/[name].[hash:7].[ext]' // option for file-loader fallback
          }
        }]
      },

      // Media
      {
        test: /\.(mp4|webm|ogg|mp3|wav|flac|aac)(\?.*)?$/,
        use: [{
          loader: 'url-loader',
          options: {
            limit: 10000,
            name: 'media/[name].[hash:7].[ext]' // option for file-loader fallback
          }
        }]
      }
    ]
  }
}
