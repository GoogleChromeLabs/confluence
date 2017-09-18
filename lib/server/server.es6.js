// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

require('./handler.es6.js');
require('./cache_handler.es6.js');
require('./file_handlers.es6.js');
require('./dao_handler.es6.js');

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'Server',
  extends: 'foam.net.node.Server',

  requires: [
    'foam.dao.QuickSink',
    'org.chromium.apis.web.CacheHandler',
    'org.chromium.apis.web.ConfluenceFileHandler',
    'org.chromium.apis.web.ConfluenceRestDAOHandler',
    'org.chromium.apis.web.ConfluenceStaticFileHandler',
  ],

  imports: ['info'],

  properties: [
    {
      class: 'FObjectArray',
      of: 'org.chromium.apis.web.Handler',
      name: 'handlers'
    },
  ],

  methods: [
    function addHandler(handler) {
      this.handlers.push(handler);
    },

    function exportDAO(dao, urlPath) {
      const handler = this.CacheHandler.create({
        delegate: this.ConfluenceRestDAOHandler.create({
          dao: dao,
          urlPath: urlPath
        }),
      });
      this.addHandler(handler);

      // NOTE: Plumbing of DAO reset events through boxes works with
      // dao.listen(sink), but NOT with dao.on.reset(listener). Use a QuickSink
      // instead of a listener.
      dao.listen(this.QuickSink.create({
        resetFn: function() {
          this.info(`DAO reset (${dao.of.id}): Clearing request cache`);
          handler.clearCache();
        }.bind(this),
      }));

      this.info(`Export DAO to ${urlPath}`);

      return handler;
    },

    function exportFile(urlPath, filePath) {
      // TODO(markdittmer): No caching in dev mode?
      const handler = this.CacheHandler.create({
        cacheSize: 1,
        delegate: this.ConfluenceFileHandler.create({
          urlPath: urlPath,
          filePath: filePath
        }),
      });
      this.handlers.push(handler);

      this.info(`Export File ${filePath} to ${urlPath}`);

      return handler;
    },

    function exportDirectory(urlPath, dir) {
      // TODO(markdittmer): No caching in dev mode?
      const handler = this.CacheHandler.create({
        delegate: this.ConfluenceStaticFileHandler.create({
          dir: dir,
          urlPath: urlPath
        }),
      });
      this.handlers.push(this.CacheHandler.create({
        delegate: this.ConfluenceStaticFileHandler.create({
          dir: dir,
          urlPath: urlPath
        }),
      }));

      this.info(`Export directory ${dir} to ${urlPath}`);
    }
  ],

  listeners: [
    function onRequest(req, res) {
      console.log('onRequest', arguments.length, req && req.constructor && req.constructor.name, res && res.constructor && res.constructor.name);
      const handlers = this.handlers;
      let i;
      for (i = 0; i < handlers.length; i++) {
        if (handlers[i].canHandleRequest(req)) {
          handlers[i].handleRequest(req, res);
          break;
        }
      }
      if (i === handlers.length) {
        res.statusCode = 404;
        res.write(foam.parsers.html.escapeString('File not found: ' + req.url),
                  'utf8');
        res.end();
      }
    }
  ]
});
