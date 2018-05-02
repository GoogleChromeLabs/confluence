// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

//
// Convert object graph JSON files to Release, WebInterface,
// ReleaseWebInterfaceJunction JSON files.
//
// Input:  ../data/object-graph/window_{browser}_{version}_{os}_{version}.json
//         files. (See ObjectGraphImporter.objectGraphPath documentation, and
//         ObjectGraphImporter.import() implementation for input details).
//
// Output: ../data/json/org.chromium.apis.web.{Release|WebInterface|ReleaseWebInterfaceJunction}.json
//

const path = require('path');

global.FOAM_FLAGS = {gcloud: true};
require('foam2');

require('../lib/web_apis/release.es6.js');
require('../lib/web_apis/release_interface_relationship.es6.js');
require('../lib/web_apis/web_interface.es6.js');
require('../lib/web_catalog/object_graph_importer.es6.js');
require('../lib/dao/dao_container.es6.js');
const pkg = org.chromium.apis.web;

//
// Setup logging, containing context, importer
//

const logger = foam.log.ConsoleLogger.create();

const container = pkg.DAOContainer.create(null, logger);
container.releaseDAO = foam.dao.MDAO.create({of: pkg.Release}, container);
container.webInterfaceDAO = foam.dao.MDAO.create({
  of: pkg.WebInterface,
}, container);
container.releaseWebInterfaceJunctionDAO = foam.dao.MDAO.create({
  of: pkg.ReleaseWebInterfaceJunction,
}, container);

const importer = pkg.ObjectGraphImporter.create({
  objectGraphPath: path.resolve(`${__dirname}/../data/object-graph`),
}, container);

//
// Import data, then store it in data/json/{class}.json
//

importer.import().then(() => {
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
  return Promise.all([
    container.releaseDAO.orderBy(pkg.Release.RELEASE_DATE).select()
        .then(store.bind(this, pkg.Release.id)),
    container.webInterfaceDAO.orderBy(pkg.WebInterface.ID).select()
        .then(store.bind(this, pkg.WebInterface.id)),
    container.releaseWebInterfaceJunctionDAO
        .orderBy(pkg.ReleaseWebInterfaceJunction.ID).select()
        .then(store.bind(this, pkg.ReleaseWebInterfaceJunction.id)),
  ]);
}).then(() => {
  logger.info(`ObjectGraph => JSON complete`);
  require('process').exit(0);
}).catch(error => {
  logger.error(`Error: ${error}
                     EXITING`);
  require('process').exit(1);
});
