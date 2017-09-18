// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

require('./handler.es6.js');

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'ConfluenceRestDAOHandler',
  extends: 'org.chromium.apis.web.Handler',
  implements: ['foam.net.node.RestDAOHandler'],

  properties: [
    {
      class: 'Array',
      name: 'payloads_',
    },
    {
      name: 'payloadPromise_',
      factory: function() {
        let resolve;
        let reject;
        let ret = new Promise(function(res, rej) {
          resolve = res;
          reject = rej;
        });
        ret.resolve_ = resolve;
        ret.reject_ = reject;
        return ret;
      },
    },
  ],

  methods: [
    function canHandleRequest(req) {
      // Check the URL for the prefix.
      const url = this.url.parse(req.url, true);
      const target = url.pathname;
      const ret = target.indexOf(this.urlPath) === 0;

      if (!ret) return ret;

      // Accumulate payload needed by handle(Request) implementation.
      let payload = '';
      req.on('data', function(chunk) {
        payload += chunk.toString();
      });
      req.on('end', function() {
        try {
          this.payloads_.push({req, payload: this.parser.parseString(payload)});
        } catch (error) {
          this.payloads_.push({req, error});
        }
      }.bind(this));

      return ret;
    },
    // TODO(markdittmer): This will repeat canHandleRequest(req) work.
    // Refactor when
    // https://groups.google.com/d/msg/foam-framework-discuss/kaFVJVeW9Ek/Dwo6Fz0RBAAJ
    // is resolved.
    function handleRequest(req, res) {
      this.handle(req, res);
    },
    function getPayload_(req) {
      const payloads = this.payloads_;
      for (let i = 0; i < payloads.length; i++) {
        if (payloads[i].req === req) {
          if (payloads[i].payload) return Promise.resolve(payloads[i].payload);
          else return Promise.reject(payloads[i].error);
        }
      }
      return Promise.reject('Missing payload for request');
    },
  ],
});
