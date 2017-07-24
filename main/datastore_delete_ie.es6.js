// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

//
// One-off script used to delete IE data from the database, as per
// https://github.com/GoogleChrome/confluence/issues/50. These data can be
// loaded again if the decision to exclude Internet Explorer is revisited.
//

const fs = require('fs');
const path = require('path');

global.FOAM_FLAGS = {gcloud: true};
require('foam2');

require('../lib/datastore/datastore_container.es6.js');
require('../lib/web_apis/release.es6.js');
require('../lib/web_apis/release_interface_relationship.es6.js');

const credentials = JSON.parse(fs.readFileSync(
    path.resolve(__dirname, '../.local/credentials.json')));

const logger =
    foam.lookup('foam.log.ConsoleLogger').create();

const datastoreCtx =
    foam.lookup('org.chromium.apis.web.DatastoreContainer').create({
      gcloudAuthEmail: credentials.client_email,
      gcloudAuthPrivateKey: credentials.private_key,
      gcloudProjectId: credentials.project_id,
      logger: logger,
    }).ctx;

const E = foam.lookup('foam.mlang.ExpressionsSingleton').create();

const ApiVelocityData = foam.lookup('org.chromium.apis.web.ApiVelocityData');
const apiVelocityDataPromise =
    datastoreCtx.apiVelocityDAO.where(E.EQ(ApiVelocityData.BROWSER_NAME, 'IE'))
    .removeAll()
    .then(() => logger.info(`Removed all API velocity data with
                                browserName="IE"`));
const BrowserMetricData =
    foam.lookup('org.chromium.apis.web.BrowserMetricData');
const browserMetricsDataPromise =
    datastoreCtx.browserMetricsDAO
    .where(E.EQ(BrowserMetricData.BROWSER_NAME, 'IE'))
    .removeAll()
    .then(() => logger.info(`Removed all browser metric data with
                                browserName="IE"`));

const Release = foam.lookup('org.chromium.apis.web.Release');
const ReleaseWebInterfaceJunction =
    foam.lookup('org.chromium.apis.web.ReleaseWebInterfaceJunction');
const QuickSink = foam.lookup('foam.dao.QuickSink');
const releaseApiJunctionAndApiPromise =
    datastoreCtx.releaseDAO.where(E.EQ(Release.BROWSER_NAME, 'IE')).select()
    .then(releaseSink => Promise.all(releaseSink.array.map(release => {
      let promises = [];
      return datastoreCtx.releaseWebInterfaceJunctionDAO.where(
          E.EQ(ReleaseWebInterfaceJunction.SOURCE_ID, release.releaseKey))
          .select(QuickSink.create({
            putFn: junction => {
              logger.info(`Removing Release/API junction: ${junction.id}`);
              promises.push(
                  datastoreCtx.releaseWebInterfaceJunctionDAO.remove(junction));
            }
          })).then(() => Promise.all(promises));
    }))).then(() => logger.info(`Removed all releases <--> API junctions with
                                    browserName="IE"`))
    // Remove interfaces that ar IE-only.
    .then(() => {
      let promises = [];
      return datastoreCtx.webInterfaceDAO.select(QuickSink.create({
        putfn: iface => {
          datastoreCtx.releaseWebInterfaceJunctionDAO.where(
              E.EQ(ReleaseWebInterfaceJunction.TARGET_ID, iface.interfaceKey))
              .select(E.COUNT()).then(countSink => {
                if (countSink.count === 0) {
                  logger.info(`Removing orphaned API: ${iface.id}`);
                  promises.push(datastoreCtx.webInterfaceDAO.remove(iface));
                }
              });
        }
      })).then(() => Promise.all(promises));
    }).then(() => logger.info(`Removed all APIs junctions that were IE-only`));

Promise.all([
  apiVelocityDataPromise,
  browserMetricsDataPromise,
  releaseApiJunctionAndApiPromise,
]).then(() => {
  datastoreCtx.releaseDAO.where(E.EQ(Release.BROWSER_NAME, 'IE')).removeAll()
      .then(() => logger.info('Removed all releases with browserName="IE"'));
});
