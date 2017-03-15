/**
 * @license
 * Copyright 2017 Google Inc. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
 'use strict';

const HtmlWebpack = require('html-webpack-plugin');
const path = require('path');
const webpack = require('webpack');
const ChunkWebpack = webpack.optimize.CommonsChunkPlugin;

const rootDir = path.resolve(__dirname, '..');

const FOAM_DIR = `${__dirname}/../node_modules/foam2`;
const execSync = require('child_process').execSync;
execSync(`node ${FOAM_DIR}/tools/build.js  web,gcloud`);

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
    }),
  ],
  resolve: {
    extensions: ['.js'],
  },
  node: {
    fs: 'empty',
    dns: 'empty',
  },
};
