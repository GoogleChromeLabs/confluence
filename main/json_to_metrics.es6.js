// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

const path = require('path');

global.FOAM_FLAGS = {gcloud: true};
require('foam2');

require('../lib/confluence/api_velocity.es6.js');
require('../lib/confluence/api_velocity_data.es6.js');
require('../lib/confluence/browser_metric_data.es6.js');
require('../lib/confluence/metric_computer_runner.es6.js');
require('../lib/dao_container.es6.js');
require('../lib/local_json_dao.es6.js');
require('../lib/web_apis/release.es6.js');
require('../lib/web_apis/release_interface_relationship.es6.js');
require('../lib/web_apis/web_interface.es6.js');
const pkg = org.chromium.apis.web;

//
// Setup logging, containing context, runner
//

const logger = foam.log.ConsoleLogger.create();

let container = pkg.DAOContainer.create(null, logger);

// Runner + ApiVelocity input: Releases that are grouped and sent to computer
// services.
container.releaseDAO = pkg.LocalJsonDAO.create({
  of: pkg.Release,
  path: `${__dirname}/../data/json/${pkg.Release.id}.json`
}, container);

// ApiVelocity input: Locally computes API count deltas.
container.webInterfaceDAO = pkg.LocalJsonDAO.create({
  of: pkg.WebInterface,
  path: `${__dirname}/../data/json/${pkg.WebInterface.id}.json`
}, container);
container.releaseWebInterfaceJunctionDAO = pkg.LocalJsonDAO.create({
  of: pkg.ReleaseWebInterfaceJunction,
  path: `${__dirname}/../data/json/${pkg.ReleaseWebInterfaceJunction.id}.json`
}, container);

// Runner output: Browser metrics.
container.browserMetricsDAO = foam.dao.MDAO.create({
  of: pkg.BrowserMetricData,
}, container);

// ApiVelocity output: API velocity data.
container.apiVelocityDAO = foam.dao.MDAO.create({
  of: pkg.ApiVelocityData,
}, container);

const runner = pkg.MetricComputerRunner.create(null, container);

//
// Compute data, then store it in data/json/{class}.json
//

Promise.all([
  runner.run(),
  pkg.ApiVelocity.create(null, container),
]).then(() => {
  const outputter = foam.json.Outputter.create({
    pretty: false,
    formatDatesAsNumbers: true,
    outputDefaultValues: false,
    useShortNames: false,
    strict: true,
  });
  function store(basename, arraySink) {
    const cls = arraySink.of || arraySink.array[0] ? arraySink.array[0].cls_ :
        foam.core.FObject;
    logger.info(`Storing ${cls.id} as ${basename}`);
    return new Promise((resolve, reject) => {
      require('fs').writeFile(
        `${__dirname}/../data/json/${basename}.json`,
        outputter.stringify(arraySink.array, cls),
        error => {
          if (error) {
            logger.error(`Error storing ${cls.id} as ${basename}`, error);
            reject(error);
          } else {
            logger.info(`Stored ${cls.id} as ${basename}`, error);
            resolve();
          }
        });
    });
  }

  const E = foam.mlang.ExpressionsSingleton.create();
  return Promise.all([
    container.browserMetricsDAO
        .orderBy(E.THEN_BY(pkg.BrowserMetricData.TYPE,
                           E.THEN_BY(pkg.BrowserMetricData.BROWSER_NAME,
                                     pkg.BrowserMetricData.DATE)))
        .select().then(store.bind(this, pkg.BrowserMetricData.id)),
    container.apiVelocityDAO
        .orderBy(E.THEN_BY(pkg.ApiVelocityData.BROWSER_NAME,
                           pkg.ApiVelocityData.RELEASE_DATE))
        .select().then(store.bind(this, pkg.ApiVelocityData.id)),
  ]);
}).then(() => {
  logger.info(`API JSON => Metrics JSON complete`);
  require('process').exit(0);
}).catch(error => {
  logger.error(`Error: ${error}
                     EXITING`);
  require('process').exit(1);
});
