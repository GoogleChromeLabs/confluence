// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

global.FOAM_FLAGS = {gcloud: true};
require('foam2');

require('../lib/compat.es6.js');
require('../lib/confluence/api_count_data.es6.js');
require('../lib/confluence/browser_metric_data.es6.js');
require('../lib/confluence/metric_computer_runner.es6.js');
require('../lib/dao/dao_container.es6.js');
require('../lib/dao/local_json_dao.es6.js');
require('../lib/data_source.es6.js');
require('../lib/web_apis/api_compat_data.es6.js');
require('../lib/web_apis/release.es6.js');
require('../lib/web_apis/release_interface_relationship.es6.js');
require('../lib/web_apis/web_interface.es6.js');
const pkg = org.chromium.apis.web;

//
// Setup logging, containing context, runner
//

// Compute metrics for non-mobile releases.
const E = foam.mlang.ExpressionsSingleton.create();
const logger = foam.log.ConsoleLogger.create(null, foam.createSubContext({
  releasePredicate: E.EQ(pkg.Release.IS_MOBILE, false),
}));

const container = pkg.DAOContainer.create(null, logger);

const compatClassURL = `file://${__dirname}/../data/json/${pkg.DAOContainer.COMPAT_MODEL_FILE_NAME}`;
pkg.ClassGenerator.create({
  classURL: compatClassURL,
}).generateClass().then((CompatData) => {
  container.releaseDAO = pkg.LocalJsonDAO.create({
    of: pkg.Release,
    path: `${__dirname}/../data/json/${pkg.Release.id}.json`,
  }, container);
  container.compatDAO = pkg.LocalJsonDAO.create({
    of: CompatData,
    path: `${__dirname}/../data/json/${CompatData.id}.json`,
  }, container);
  container.browserMetricsDAO = foam.dao.MDAO.create({
    of: pkg.BrowserMetricData,
  }, container);
  container.apiCountDAO = foam.dao.MDAO.create({
    of: pkg.ApiCountData,
  }, container);

  const runner = pkg.MetricComputerRunner.create({
    mode: pkg.DataSource.LOCAL,
  }, container);

  //
  // Compute data, then store it in data/json/{class}.json
  //

  return runner.run().then(() => {
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
            (error) => {
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

    return Promise.all([
      container.browserMetricsDAO
          .orderBy(E.THEN_BY(pkg.BrowserMetricData.TYPE,
              E.THEN_BY(pkg.BrowserMetricData.BROWSER_NAME,
                  pkg.BrowserMetricData.DATE)))
          .select().then(store.bind(this, pkg.BrowserMetricData.id)),
      container.apiCountDAO
          .orderBy(E.THEN_BY(pkg.ApiCountData.BROWSER_NAME,
              pkg.ApiCountData.RELEASE_DATE))
          .select().then(store.bind(this, pkg.ApiCountData.id)),
    ]);
  }).then(() => {
    logger.info(`API JSON => Metrics JSON complete`);
    require('process').exit(0);
  });
}).catch((error) => {
  logger.error(`Error: ${error}
                    EXITING`);
  require('process').exit(1);
});
