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
  devServer: {
    contentBase: path.resolve(rootDir, 'dist'),
    port: 9000,
  },
  entry: {
    app: [path.resolve(rootDir, 'main', 'main.es6')],
    foam: [path.resolve(rootDir, 'node_modules/foam2', 'foam-bin')],
  },
  output: {
      filename: '[name].bundle.js',
      path: path.resolve(rootDir, 'dist'),
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
      filename: 'vendor.bundle.js',
      minChunks: Infinity,
      name: 'vendor',
    }),
    new HtmlWebpack({
      filename: 'index.html',
      inject: 'body',
      template: path.resolve(rootDir, 'static', 'index.html'),
    }),
    new webpack.ContextReplacementPlugin(
      /angular(\\|\/)core(\\|\/)(esm(\\|\/)src|src)(\\|\/)linker/,
      __dirname
    ),
  ],
  resolve: {
    extensions: ['.js'],
  },
  node: {
    fs: 'empty',
    dns: 'empty',
  },
};
