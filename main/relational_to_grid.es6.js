// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

const fs = require('fs');
const path = require('path');
const process = require('process');
const url = require('url');

require('foam2');

require('../lib/dao/grid_dao.es6.js');
require('../lib/dao/http_json_dao.es6.js');
require('../lib/dao/json_dao_container.es6.js');
require('../lib/dao/local_json_dao.es6.js');
require('../lib/web_apis/release.es6.js');
require('../lib/web_apis/release_interface_relationship.es6.js');
require('../lib/web_apis/web_interface.es6.js');
const pkg = org.chromium.apis.web;

const logger = foam.log.ConsoleLogger.create();

const USAGE = `Transforms (Release, API) pair relations to instances of
org.chromium.apis.web.GridRow. Input and output files are
[class package path].json in [repository root]/data.

USAGE:

    node /path/to/relationship_to_grid.es6.js BaseConfluenceDataURL

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
  basename: dataUrl.protocol === 'file:' ? dataUrl.pathname : url.format(dataUrl),
}, logger);

logger.info('Gathering Confluence data from JSON');

const outputter = foam.json.Outputter.create({
  pretty: false,
  formatDatesAsNumbers: true,
  outputDefaultValues: false,
  useShortNames: false,
  strict: true,
});
function store(arraySink) {
  const cls = arraySink.of || arraySink.array[0] ? arraySink.array[0].cls_ :
      foam.core.FObject;
  logger.info(`Storing ${cls.id}`);
  return new Promise((resolve, reject) => {
    fs.writeFile(
        `${__dirname}/../data/json/${cls.id}.json`,
        outputter.stringify(arraySink.array, cls),
        error => {
          if (error) {
            logger.error(`Error storing ${cls.id}`, error);
            reject(error);
          } else {
            logger.info(`Stored ${cls.id}`);
            resolve();
          }
        });
    });
}

Promise.all([
  container.releaseDAO.orderBy(pkg.Release.RELEASE_DATE).select(),
  container.webInterfaceDAO.orderBy(pkg.WebInterface.ID).select(),
  container.releaseWebInterfaceJunctionDAO.select(),
]).then(function(arraySinks) {
  logger.info('JSON data gathered');

  const releases = arraySinks[0].array;
  const apis = arraySinks[1].array;
  const joins = arraySinks[2].array;

  logger.info('Indexing data');
  let releaseIdxs = {};
  for (let i = 0; i < releases.length; i++) {
    releaseIdxs[releases[i].id] = i;
  }
  let rows = new Array(apis.length);
  let apiIdxs = {};
  for (let i = 0; i < apis.length; i++) {
    rows[i] = pkg.GridRow.create({id: apis[i].id, data: new Array(releases.length)});
    for (let j = 0; j < releases.length; j++) {
      rows[i].data[j] = false;
    }
    apiIdxs[apis[i].id] = i;
  }
  logger.info('Data indexed');

  logger.info('Storing data grid');
  for (const join of joins) {
    const row = rows[apiIdxs[join.targetId]];
    row.data[releaseIdxs[join.sourceId]] = true;
  }

  return store(foam.dao.ArraySink.create({of: pkg.GridRow, array: rows}));
}).then(() => {
  logger.info('Grid data stored');
  process.exit(0);
}).catch(error => {
  logger.error(`ERROR: ${error}`);
  process.exit(1);
});
