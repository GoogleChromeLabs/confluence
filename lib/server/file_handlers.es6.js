// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

require('./handler.es6.js');

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'ConfluenceFileHandler',
  extends: 'org.chromium.apis.web.Handler',
  implements: ['foam.net.node.FileHandler'],

  methods: [
    function canHandleRequest(req) {
      return req.url === this.urlPath;
    },
    // TODO(markdittmer): This will repeat canHandleRequest(req) work.
    // Refactor when
    // https://groups.google.com/d/msg/foam-framework-discuss/kaFVJVeW9Ek/Dwo6Fz0RBAAJ
    // is resolved.
    function handleRequest(req, res) { this.handle(req, res); }
  ],
});

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'ConfluenceStaticFileHandler',
  extends: 'org.chromium.apis.web.Handler',
  implements: ['foam.net.node.StaticFileHandler'],

  properties: [
    {
      class: 'String',
      name: 'dir',
      documentation: 'Directory under which to serve files.',
      preSet: function(old, nu) {
        return this.path.resolve(process.cwd(), nu);
      },
      factory: function() { return process.cwd(); }
    },
    {
      class: 'String',
      name: 'urlPath',
      documentation: 'URL path prefix. Stripped before searching "dir".'
    },
    {
      name: 'mimeTypes',
      factory: function() {
        return {
          '.js': 'text/javascript',
          '.css': 'text/css',
          '.html': 'text/html',
          __default: 'application/octet-stream'
        };
      }
    },
    {
      name: 'path',
      factory: function() { return require('path'); }
    },
    {
      name: 'fs',
      factory: function() { return require('fs'); }
    }
  ],

  methods: [
    function canHandleRequest(req) {
      if (!this.dir) return false;

      // Check the URL for the prefix.
      const target = this.getPathTarget_(req.url);
      if (target === null) return false;
      return true;
    },
    // TODO(markdittmer): This will repeat canHandleRequest(req) work.
    // Refactor when
    // https://groups.google.com/d/msg/foam-framework-discuss/kaFVJVeW9Ek/Dwo6Fz0RBAAJ
    // is resolved.
    function handleRequest(req, res) { this.handle(req, res); },
    function getPathTarget_(url) {
      // Check the URL for the prefix.
      let target = url;
      if (target.indexOf(this.urlPath) !== 0) return null;

      target = target.substring(this.urlPath.length);

      // Check and strip the prefix off the URL.
      if (target.indexOf('?') >= 0)
        target = target.substring(0, target.indexOf('?'));
      if (target.indexOf('#') >= 0)
        target = target.substring(0, target.indexOf('#'));

      // String a leading slash, if any.
      if (target.charAt(0) === '/') target = target.substring(1);

      return target || null;
    },
  ],
});
