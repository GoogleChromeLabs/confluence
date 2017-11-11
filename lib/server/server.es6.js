// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

foam.ENUM({
  package: 'org.chromium.apis.web',
  name: 'ServerMode',

  values: ['DEV', 'PROD'],
});

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'Server',
  extends: 'foam.net.node.Server',
  implements: ['foam.mlang.Expressions'],

  requires: [
    'foam.dao.LRUDAOManager',
    'foam.net.node.CacheHandler',
    'foam.net.node.DirTreeHandler',
    'foam.net.node.FileHandler',
    'foam.net.node.PathnameRouter',
    'foam.net.node.RestDAOHandler',
    'org.chromium.apis.web.BrowserMetricData',
    'org.chromium.apis.web.BrowserMetricDataType',
    'org.chromium.apis.web.DAOContainer',
    'org.chromium.apis.web.ServerMode',
  ],
  imports: [
    'apiVelocityDAO',
    'browserMetricsDAO',
    'releaseDAO',
    'releaseWebInterfaceJunctionDAO',
    'webInterfaceDAO',
  ],
  exports: ['cache_ as requestCacheDAO'],

  classes: [
    {
      name: 'CacheDAO',
      extends: 'foam.dao.ProxyDAO',

      requires: [
        'foam.net.node.CachedResponse',
        'foam.dao.MDAO',
      ],
      imports: ['info'],

      properties: [
        {
          name: 'of',
          factory: function() { return this.CachedResponse; },
        },
        {
          name: 'delegate',
          factory: function() {
            return this.MDAO.create({of: this.CachedResponse});
          },
        },
      ],

      methods: [
        function put_(ctx, cachedResponse) {
          // Cache successes.
          if (cachedResponse.statusCode >= 200 &&
              cachedResponse.statusCode < 300) {
            return this.delegate.put_(ctx, cachedResponse);
          }

          // Non-cached does not throw, but does not get stored either.
          this.info(`CacheDAO: Bypass cache storage: ${cachedResponse.id}`);
          return Promise.resolve(cachedResponse);
        },
      ],
    },
  ],

  properties: [
    {
      class: 'Enum',
      of: 'org.chromium.apis.web.ServerMode',
      name: 'mode',
      required: true,
      final: true
    },
    {
      class: 'FObjectProperty',
      of: 'foam.net.node.PathnameRouter',
      name: 'router',
      documentation: `Router for registering routes.`,
      factory: function() {
        return this.PathnameRouter.create();
      }
    },
    {
      class: 'FObjectProperty',
      of: 'foam.net.node.Handler',
      name: 'handler',
      documentation: `Server handler: Cache everything registered to router.`,
      factory: function() {
        // Non-prod server => no caching.
        return this.mode === this.ServerMode.PROD ? this.CacheHandler.create({
          delegate: this.router
        }) : this.router;
      },
    },
    {
      name: 'path',
      factory: function() { return require('path'); },
    },
    {
      class: 'foam.dao.DAOProperty',
      name: 'cache_',
    },
    {
      class: 'FObjectProperty',
      of: 'foam.dao.LRUDAOManager',
      name: 'cacheManager_',
    },
  ],

  methods: [
    function start() {

      // Initialize predictably on start().
      this.cache_ = this.CacheDAO.create();
      this.cacheManager_ = this.LRUDAOManager.create({
        dao: this.cache_,
        maxSize: 50,
      });

      this.addDir_('/images',
                   this.path.resolve(__dirname, '../../static/images'));
      this.addDir_('/bundle',
                   this.path.resolve(__dirname, '../../static/bundle'));
      this.addFile_('/blank.html',
                    this.path.resolve(__dirname, '../../static/blank.html'));
      this.addFile_('/index.html',
                    this.path.resolve(__dirname, '../../static/index.html'));
      this.addFile_('/',
                    this.path.resolve(__dirname, '../../static/index.html'));

      const EQ = this.EQ.bind(this);
      const Type = this.BrowserMetricDataType;
      const TYPE = this.BrowserMetricData.TYPE;

      this.addDAORoute_(this.DAOContainer.RELEASE_NAME, this.releaseDAO);
      this.addDAORoute_(this.DAOContainer.WEB_INTERFACE_NAME,
                       this.webInterfaceDAO);
      this.addDAORoute_(this.DAOContainer.RELEASE_WEB_INTERFACE_JUNCTION_NAME,
                       this.releaseWebInterfaceJunctionDAO);

      this.addDAORoute_(
          this.DAOContainer.FAILURE_TO_SHIP_NAME,
          this.browserMetricsDAO.where(EQ(TYPE, Type.FAILURE_TO_SHIP)));
      this.addDAORoute_(
          this.DAOContainer.BROWSER_SPECIFIC_NAME,
          this.browserMetricsDAO.where(EQ(TYPE, Type.BROWSER_SPECIFIC)));
      this.addDAORoute_(
          this.DAOContainer.AGGRESSIVE_REMOVAL_NAME,
          this.browserMetricsDAO.where(EQ(TYPE, Type.AGGRESSIVE_REMOVAL)));
      this.addDAORoute_(
          this.DAOContainer.API_VELOCITY_NAME, this.apiVelocityDAO);

      this.validate();

      this.SUPER();
    },
    function addFile_(pathname, filePath) {
      this.router.addPathname(pathname, this.FileHandler.create({
        filePath,
      }));
    },
    function addDir_(pathnamePrefix, dir) {
      this.router.addPathnamePrefix(pathnamePrefix, this.DirTreeHandler.create({
        dir,
      }));
    },
    function addDAORoute_(daoName, dao) {
      this.router.addPathnamePrefix(`/${daoName}`, this.RestDAOHandler.create({
        dao,
      }));
    },
  ],
});
