'use strict';

const path = require('path');

const webpack = require('webpack');

module.exports = {
  devtool: 'eval-source-map',

  devServer: {
    contentBase: '.',
    port: 4008,
    stats: 'errors-only'
  },

  entry: './src/Main.purs',

  output: {
    path: __dirname,
    pathinfo: true,
    filename: 'bundle.js'
  },

  module: {
    rules: [
      {
        test: /\.purs$/,
        use: [
          {
            loader: 'purs-loader',
            options: {
              src: [
                'bower_components/purescript-*/src/**/*.purs',
                'src/**/*.purs'
              ],
              bundle: false,
              psc: 'psa',
              watch: true,
              pscIde: false
            }
          }
        ]
      },
    ]
  },

  resolve: {
    modules: [ 'node_modules', 'bower_components' ],
    extensions: [ '.purs', '.js']
  },

  plugins: [
    new webpack.LoaderOptionsPlugin({
      debug: true
    })
  ]
};
