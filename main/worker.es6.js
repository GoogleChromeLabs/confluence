// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

self.window = self.global = self;
importScripts('vendors.bundle.js', 'foam.bundle.js');

require('../lib/client/api_confluence.es6.js');
require('../lib/client/api_matrix.es6.js');
require('../lib/client/events.es6.js');
require('../lib/confluence/api_velocity_data.es6.js');
require('../lib/confluence/browser_metric_data.es6.js');
require('../lib/dao/dao_container.es6.js');
require('../lib/web_apis/release.es6.js');
require('../lib/web_apis/release_interface_relationship.es6.js');
require('../lib/web_apis/web_interface.es6.js');
const pkg = org.chromium.apis.web;

//
// Box context setup.
//

const ctx = foam.box.Context.create({
  myname: '/worker',
  unsafe: false,
  classWhitelist: require('../data/class_whitelist.json'),
});

//
// DAO setup.
//

function getCachingDAO(name, cls, ctx) {
  return foam.dao.CachingDAO.create({
    src: foam.dao.RestDAO.create({
      baseURL: `${self.location.origin}/${name}`,
      of: cls,
    }, ctx),
    cache: foam.dao.MDAO.create({of: cls}, ctx),
  }, ctx);
}

function getLazyCacheDAO(name, cls, ctx) {
  return foam.dao.LazyCacheDAO.create({
    delegate: foam.dao.RestDAO.create({
      baseURL: `${self.location.origin}/${name}`,
      of: cls,
    }, ctx),
    cache: foam.dao.MDAO.create({of: cls}, ctx),
    cacheOnSelect: true,
    staleTimeout: Infinity,
  }, ctx);
}

const C = pkg.DAOContainer;

const releaseDAO = getCachingDAO(
    C.RELEASE_NAME, pkg.Release, ctx);
const webInterfaceDAO = getCachingDAO(
    C.WEB_INTERFACE_NAME, pkg.WebInterface, ctx);
const apiVelocityDAO = getCachingDAO(
    C.API_VELOCITY_NAME, pkg.ApiVelocityData, ctx);
const failureToShipDAO = getCachingDAO(
    C.FAILURE_TO_SHIP_NAME, pkg.BrowserMetricData, ctx);
const browserSpecificDAO = getCachingDAO(
    C.BROWSER_SPECIFIC_NAME, pkg.BrowserMetricData, ctx);
const aggressiveRemovalDAO = getCachingDAO(
    C.AGGRESSIVE_REMOVAL_NAME, pkg.BrowserMetricData, ctx);

const releaseWebInterfaceJunctionDAO = getLazyCacheDAO(
    C.RELEASE_WEB_INTERFACE_JUNCTION_NAME,
    pkg.ReleaseWebInterfaceJunction,
    ctx);

const daoCtx = ctx.__subContext__.createSubContext({
  releaseDAO,
  webInterfaceDAO,
  releaseWebInterfaceJunctionDAO,
});


//
// Setup services for page.
//

const apiMatrixController = pkg.ApiMatrixController.create(null, daoCtx);
const apiConfluence = pkg.ApiConfluence.create({
  apiVelocityDAO,
  failureToShipDAO,
  browserSpecificDAO,
  aggressiveRemovalDAO,
}, daoCtx);

// Register API under known names.
ctx.registry.register('apiMatrixController', null, foam.box.SkeletonBox.create({
  data: apiMatrixController,
}));
ctx.registry.register('apiConfluence', null, foam.box.SkeletonBox.create({
  data: apiConfluence,
}));

// Now that services are exported, begin accepting messages from page.
onmessage = function(event) {
  if (!event.data instanceof MessagePort) {
    throw new Error('Unexpected control message', event.data);
  }

  ctx.messagePortService.addPort(event.data);
};

//
// Bind to services from page.
//

// Bind to page events service once service has established MessagePort
// connection.
const events = foam.box.PromisedBox.create({
  delegate: new Promise(function(resolve, reject) {
    ctx.messagePortService.connect.sub(function(_, __, namedBox) {
      if (namedBox.name !== '/page') {
        reject(new Error(`Unexpected MessagePort connection from:
                              ${namedBox.name}`));
        return;
      }
      resolve(foam.box.NamedBox.create({name: '/page/events'}, ctx));
    });
  }),
}, ctx);

// Forward events to page.
apiMatrixController.events.sub(function(_, __, event) {
  events.send(foam.box.Message.create({
    object: foam.box.EventMessage.create({
      args: [event],
    }, ctx),
  }, ctx));
});
