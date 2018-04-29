// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

foam.CLASS({
  name: 'ClassGenerator',
  package: 'org.chromium.apis.web',

  axioms: [
    foam.pattern.Multiton.create({property: 'classURL'}),
  ],

  requires: [
    'foam.box.ClassWhitelistContext',
    'foam.net.HTTPRequest',
    'foam.json.Parser',
  ],

  properties: [
    {
      class: 'String',
      name: 'classURL',
      final: true,
      required: true,
    },
    {
      class: 'Function',
      name: 'parseURL_',
      transient: true,
      factory: function() {
        if (foam.isServer) {
          const url = require('url');
          return url.parse.bind(url);
        } else {
          return str => new URL(str);
        }
      },
    },
    {
      class: 'FObjectProperty',
      of: 'foam.json.Parser',
      name: 'parser',
      transient: true,
      factory: function() {
        return this.Parser.create({
          strict: true,
          creationContext: this.ClassWhitelistContext.create({
            whitelist: require('../data/class_whitelist.json'),
          }, this).__subContext__,
        });
      },
    },
    {
      name: 'fs_',
      transient: true,
      factory: function() {
        foam.assert(
            foam.isServer,
            'Attempted filesystem access from non-NodeJS context');
        return require('fs');
      },
    },
    {
      name: 'clsPromise_',
      transient: true,
      value: null,
    },
  ],

  methods: [
    {
      name: 'generateClass',
      code: function() {
        this.validate();

        if (this.clsPromise_) {
          return this.clsPromise_;
        }

        return this.clsPromise_ = this.loadSpec_().then(specStr => {
          const model = this.parser.parseClassFromString(
              specStr, foam.core.Model);
          model.validate();
          const cls = model.buildClass();
          cls.validate();
          foam.register(cls);
          foam.package.registerClass(cls);
          return cls;
        });
      },
    },
    {
      name: 'loadSpec_',
      code: function() {
        let url;
        try {
          url = this.parseURL_(this.classURL);
        } catch (err) {
          return Promise.reject(err);
        }
        if (url.protocol === 'https:') {
          return this.HTTPRequest.create({
            url: this.classURL,
          }).send().then(resp => {
            return resp.payload;
          }).then(strOrBuffer => {
            return strOrBuffer.toString();
          });
        } else if (url.protocol === 'file:') {
          return new Promise((resolve, reject) => {
            this.fs_.readFile(url.pathname, (err, data) => {
              if (err) {
                reject(err);
              } else {
                resolve(data.toString());
              }
            })
          });
        } else {
          return Promise.reject(
              new Error(`Invalid URL protocol: "${url.protocol}"`));
        }
      },
    },
  ],
});
