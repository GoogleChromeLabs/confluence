// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

const fs = require('fs');
const path = require('path');
const process = require('process');
const url = require('url');

require('foam2');

require('../lib/property.es6.js');

require('../lib/dao/grid_dao.es6.js');
require('../lib/dao/http_json_dao.es6.js');
require('../lib/dao/json_dao_container.es6.js');
require('../lib/dao/local_json_dao.es6.js');
require('../lib/web_apis/api_compat_data.es6.js');
require('../lib/web_apis/relational_to_compat.es6.js');
require('../lib/web_apis/release.es6.js');
require('../lib/web_apis/release_interface_relationship.es6.js');
require('../lib/web_apis/web_interface.es6.js');
const pkg = org.chromium.apis.web;

const logger = foam.log.ConsoleLogger.create();

const USAGE = `Transforms (Release, API) pair relations to instances of
org.chromium.apis.web.generated.CompatData. Input and output files are
[class package path].json in [repository root]/data, with class model files in
class:[class package path].json.

USAGE:

    node /path/to/relationship_to_compat.es6.js BaseConfluenceDataURL

        BaseConfluenceDataURL = absolute https: or file: URL to directory
                                where Confluence data are stored with NO
                                trailing slash.`;
if (process.argv.length !== 3) {
  console.error(USAGE);
  process.exit(1);
}

const dataUrl = url.parse(process.argv[2]);
if (dataUrl.protocol !== 'file:' && dataUrl.protocol !== 'https:') {
  console.error('BaseConfluenceDataURL parameter must be file: or https: URL');
}

let container = pkg.JsonDAOContainer.create({
  mode: dataUrl.protocol === 'file:' ? pkg.JsonDAOContainerMode.LOCAL :
      pkg.JsonDAOContainerMode.HTTP,
  basename: dataUrl.protocol === 'file:' ? dataUrl.pathname :
      url.format(dataUrl),
}, logger);

logger.info('Gathering Confluence data from JSON');

const dataOutputter = foam.json.Outputter.create({
  passPropertiesByReference: true,
  pretty: false,
  formatDatesAsNumbers: true,
  outputDefaultValues: false,
  useShortNames: false,
  strict: true,
});
function storeDataFromArray(array, cls) {
  logger.info(`Storing collection of ${cls.id}`);
  return new Promise((resolve, reject) => {
    fs.writeFile(
        `${__dirname}/../data/json/${cls.id}.json`,
        dataOutputter.stringify(array, cls),
        error => {
          if (error) {
            logger.error(`Error storing collection of ${cls.id}`, error);
            reject(error);
          } else {
            logger.info(`Stored collection of ${cls.id}`);
            resolve();
          }
        });
    });
}

const modelOutputter = foam.json.Outputter.create({
  passPropertiesByReference: false,
  pretty: false,
  formatDatesAsNumbers: true,
  outputDefaultValues: false,
  useShortNames: false,
  strict: true,
});
function storeClassModel(model) {
  logger.info(`Storing class model for ${model.id}`);
  return new Promise((resolve, reject) => {
    fs.writeFile(
        `${__dirname}/../data/json/class:${model.id}.json`,
        modelOutputter.stringify(model, foam.core.Model),
        error => {
          if (error) {
            logger.error(`Error storing model for ${model.id}`, error);
            reject(error);
          } else {
            logger.info(`Stored model for ${model.id}`);
            resolve();
          }
        });
    });
}

Promise.all([
  // Order by release date, falling back on browser, os data.
  container.releaseDAO
      .orderBy(pkg.Release.OS_VERSION)
      .orderBy(pkg.Release.OS_NAME)
      .orderBy(pkg.Release.BROWSER_VERSION)
      .orderBy(pkg.Release.BROWSER_NAME)
      .orderBy(pkg.Release.RELEASE_DATE)
      .select(),
  container.webInterfaceDAO.orderBy(pkg.WebInterface.ID).select(),
  container.releaseWebInterfaceJunctionDAO.select(),
]).then(function(arraySinks) {
  logger.info('JSON data gathered');

  const releases = arraySinks[0].array;
  const apis = arraySinks[1].array;
  const joins = arraySinks[2].array;

  const clsAndData = pkg.RelationalToCompatConverter.create(null, logger)
      .convert(releases, apis, joins);

  logger.info('Storing new data model and collection');
  return Promise.all([
    storeClassModel(clsAndData.cls.model_),
    storeDataFromArray(clsAndData.data, clsAndData.cls),
  ]);
}).then(() => {
  logger.info('New data model and collection stored');
  process.exit(0);
}).catch(error => {
  logger.error(`ERROR: ${error}`);
  process.exit(1);
});
