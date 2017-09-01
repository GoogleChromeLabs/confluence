// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

self.window = self.global = self;
importScripts('foam.bundle.js');

require('../lib/client/api_matrix.es6.js');
require('../lib/client/events.es6.js');
require('../lib/confluence/api_velocity_data.es6.js');
require('../lib/confluence/browser_metric_data.es6.js');
require('../lib/dao_container.es6.js');
require('../lib/web_apis/release.es6.js');
require('../lib/web_apis/release_interface_relationship.es6.js');
require('../lib/web_apis/web_interface.es6.js');
const pkg = org.chromium.apis.web;

//
// Box context setup.
//

const ctx = foam.box.Context.create({
  myname: '/sync_worker',
});

//
// DAO services setup.
//

// Expose DAO in box context.
function exportDAO(name, dao) {
  return ctx.registry.register(name, null, foam.box.SkeletonBox.create({
    data: dao,
  }));
}

// Get a DAO for a given endpoint name, unversioned class, versioned class,
// and context.
function getDAO(name, cls, versionedCls, ctx) {
  const dao = foam.dao.AdapterDAO.create({
    of: cls,
    to: versionedCls,
    delegate: foam.dao.SyncDAO.create({
      of: versionedCls,
      remoteDAO: foam.dao.RestDAO.create({
        baseURL: `${self.location.origin}/${name}`,
        of: cls,
      }, ctx),
      delegate: foam.dao.MDAO.create({of: versionedCls}, ctx),
      syncRecordDAO: foam.dao.MDAO.create({
        of: foam.dao.sync.VersionedSyncRecord,
      }, ctx),
      // Poll every 20min.
      polling: true,
      pollingFrequency: 1000 * 60 * 20,
    }, ctx),
  }, ctx);
  exportDAO(name, dao);
  return dao;
}

const C = pkg.DAOContainer;
const releaseDAO = getDAO(C.RELEASE_NAME,
                          pkg.Release,
                          pkg.VersionedRelease,
                          ctx);
const webInterfaceDAO = getDAO(C.WEB_INTERFACE_NAME,
                               pkg.WebInterface,
                               pkg.VersionedWebInterface,
                               ctx);
const apiVelocityDAO = getDAO(C.API_VELOCITY_NAME,
                              pkg.ApiVelocityData,
                              pkg.VersionedApiVelocityData,
                              ctx);
const failureToShipDAO = getDAO(C.FAILURE_TO_SHIP_NAME,
                                pkg.BrowserMetricData,
                                pkg.VersionedBrowserMetricData,
                                ctx);
const browserSpecificDAO = getDAO(C.BROWSER_SPECIFIC_NAME,
                                  pkg.BrowserMetricData,
                                  pkg.VersionedBrowserMetricData,
                                  ctx);
const aggressiveRemovalDAO = getDAO(C.AGGRESSIVE_REMOVAL_NAME,
                                    pkg.BrowserMetricData,
                                    pkg.VersionedBrowserMetricData,
                                    ctx);

// Use common cache for junction DAO, even though each release's API data is
// loaded separately.
const junctionCacheDAO = foam.dao.MDAO.create({
  of: pkg.VersionedReleaseWebInterfaceJunction,
}, ctx);
const junctionDAO = foam.dao.AdapterDAO.create({
  of: pkg.ReleaseWebInterfaceJunction,
  to: pkg.VersionedReleaseWebInterfaceJunction,
  delegate: junctionCacheDAO,
});
const junctionRemoteDAO = foam.dao.RestDAO.create({
  baseURL: `${self.location.origin}/${C.RELEASE_WEB_INTERFACE_JUNCTION_NAME}`,
  of: pkg.VersionedReleaseWebInterfaceJunction,
}, ctx);

exportDAO(C.RELEASE_WEB_INTERFACE_JUNCTION_NAME, junctionCacheDAO);

//
// Page service exchange setup.
//

// Now that services are exported, begin accepting messages from page.
onmessage = function(event) {
  if (!event.data instanceof MessagePort) {
    throw new Error('Unexpected control message', event.data);
  }

  ctx.messagePortService.addPort(event.data);
};

// Bind to page events service once service has established MessagePort
// connection.
const events = foam.box.PromisedBox.create({
  delegate: new Promise(function(resolve, reject) {
    ctx.messagePortService.connect.sub(function(_, __, namedBox) {
      if (namedBox.name !== '/page') {
        reject(new Error(`Unexpected MessagePort connection from: ${namedBox.name}`));
        return;
      }
      resolve(foam.box.NamedBox.create({name: '/page/events'}, ctx));
    });
  }),
}, ctx);

//
// Data loading.
//

// Wait for data to be loaded from DAO, then fire loaded event.
function notifyDAOLoaded(name, opt_predicate) {
  events.send(foam.box.Message.create({
    object: foam.box.EventMessage.create({
      args: [pkg.DAOSyncedEvent.create({
        name: name,
        predicate: opt_predicate || null,
      }, ctx)],
    }, ctx),
  }, ctx));
}

const E = foam.mlang.ExpressionsSingleton.create();

// Load releases first.
releaseDAO.delegate.synced.then(() => {
  notifyDAOLoaded(C.RELEASE_NAME);
  const ApiMatrix = pkg.ApiMatrix;
  return ApiMatrix.SEPARATE_LATEST_RELEASES(releaseDAO).then(latestAndRest => {
    const latestReleases = latestAndRest.latest;
    const restReleases = latestAndRest.rest;


    // First, load latest releases, then the rest.
    return Promise.all(latestReleases.map(release => {
      const predicate = E.EQ(
          pkg.VersionedReleaseWebInterfaceJunction.SOURCE_ID,
          release.id);
      // Pipe release/API data in to local cache, then notify data ready.
      return junctionRemoteDAO.where(predicate).select(foam.dao.DAOSink.create({
        dao: junctionCacheDAO,
      })).then(notifyDAOLoaded.bind(
          this,
          C.RELEASE_WEB_INTERFACE_JUNCTION_NAME,
          predicate));
    })).then(() => {
      // After loading initial releases' API data, load the rest.
      function loadNext(i) {
        const release = restReleases[i];
        if (!release) return undefined;

        const predicate = E.EQ(
            pkg.VersionedReleaseWebInterfaceJunction.SOURCE_ID,
            release.id);
        // Pipe release/API data in to local cache, then notify data ready.
        return junctionRemoteDAO.where(predicate).select(foam.dao.DAOSink.create({
          dao: junctionCacheDAO,
        })).then(notifyDAOLoaded.bind(
            this,
            C.RELEASE_WEB_INTERFACE_JUNCTION_NAME,
            predicate));
        // When in series:
        // .then(loadNext.bind(this, i + 1));
      }
      // Load remaining data in parallel.
      return Promise.all(restReleases.map((_, i) => loadNext(i)));
    });
  }).then(() => {
    const versionedCls = pkg.VersionedReleaseWebInterfaceJunction;
    // Store global reference to SyncDAO so it doesn't get garbage collected.
    window[C.RELEASE_WEB_INTERFACE_JUNCTION_NAME] =
        foam.dao.SyncDAO.create({
          of: versionedCls,
          remoteDAO: junctionRemoteDAO,
          delegate: foam.dao.MDAO.create({of: versionedCls}, ctx),
          syncRecordDAO: foam.dao.MDAO.create({
            of: foam.dao.sync.VersionedSyncRecord,
          }, ctx),
          // Poll every 20min.
          polling: true,
          pollingFrequency: 1000 * 60 * 20,
        }, ctx);
  });
});

// Load API and metrics data immediately.
webInterfaceDAO.delegate.synced
    .then(() => notifyDAOLoaded(C.WEB_INTERFACE_NAME));
apiVelocityDAO.delegate.synced
    .then(() => notifyDAOLoaded(C.API_VELOCITY_NAME));
failureToShipDAO.delegate.synced
    .then(() => notifyDAOLoaded(C.FAILURE_TO_SHIP_NAME));
browserSpecificDAO.delegate.synced
    .then(() => notifyDAOLoaded(C.BROWSER_SPECIFIC_NAME));
aggressiveRemovalDAO.delegate.synced
    .then(() => notifyDAOLoaded(C.AGGRESSIVE_REMOVAL_NAME));
