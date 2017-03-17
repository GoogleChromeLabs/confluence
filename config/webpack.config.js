// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

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
  ],
  resolve: {
    extensions: ['.js'],
  },
  node: {
    fs: 'empty',
    dns: 'empty',
  },
};
