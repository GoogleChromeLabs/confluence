'use strict';

const HtmlWebpack = require('html-webpack-plugin');
const path = require('path');
const webpack = require('webpack');
const ChunkWebpack = webpack.optimize.CommonsChunkPlugin;

const rootDir = path.resolve(__dirname, '..');

const FOAM_DIR = `${__dirname}/../node_modules/foam2`;
const execSync = require('child_process').execSync;
execSync(`node ${FOAM_DIR}/tools/build.js web`);

module.exports = {
  entry: {
    foam: [path.resolve(rootDir, 'node_modules/foam2', 'foam-bin')],
  },
  output: {
    filename: '[name].bundle.js',
    path: path.resolve(rootDir, 'static/bundle'),
  },
  module: {
    loaders: [
      {
        test: /\.es6\.js$/,
        loader: 'babel-loader',
        query: {
          presets: ['es2015'],
          plugins: ['transform-runtime'],
        },
      },
    ],
  },
  plugins: [
    new ChunkWebpack({
      filename: 'vendors.bundle.js',
      minChunks: Infinity,
      name: 'vendors',
    }),
    new HtmlWebpack({
      filename: 'index.html',
      inject: 'body',
      template: path.resolve(rootDir, 'static', 'index.html'),
    })
  ],
  resolve: {
    extensions: ['.js'],
  },
  node: {
    fs: 'empty',
    dns: 'empty',
  },
};
