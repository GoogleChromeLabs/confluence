// Copyright 2018 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

const fs = require('fs');

global.FOAM_FLAGS = {gcloud: true};
require('foam2');

require('../lib/compat.es6.js');
require('../lib/confluence/api_count_data.es6.js');
require('../lib/confluence/browser_metric_data.es6.js');
require('../lib/dao/dao_container.es6.js');
require('../lib/dao/local_json_dao.es6.js');
require('../lib/data_source.es6.js');
require('../lib/parse/expressions.es6.js');
require('../lib/web_apis/api_compat_data.es6.js');
require('../lib/web_apis/api_interop_data.es6.js');
require('../lib/web_apis/release_interface_relationship.es6.js');
require('../lib/web_apis/release.es6.js');
require('../lib/web_apis/web_interface.es6.js');
const pkg = org.chromium.apis.web;

//
// Setup logging, containing context
//

// Store data for non-mobile releases.
const E = foam.mlang.ExpressionsSingleton.create();
const logger = foam.log.ConsoleLogger.create(null, foam.createSubContext({
  releasePredicate: E.EQ(pkg.Release.IS_MOBILE, false),
}));

let container = pkg.DAOContainer.create(null, logger);

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

(async () => {
  const compatClassURL = `file://${__dirname}/../data/json/${pkg.DAOContainer.COMPAT_MODEL_FILE_NAME}`;
  const gen = pkg.AbstractCompatClassGenerator.create(null, container);
  const CompatData = await pkg.ClassGenerator.create({
    classURL: compatClassURL,
  }, container).generateClass();

  const releaseDAO = container.releaseDAO = pkg.LocalJsonDAO.create({
    of: pkg.Release,
    path: `${__dirname}/../data/json/${pkg.Release.id}.json`
  }, container);
  const compatDAO = container.compatDAO = pkg.LocalJsonDAO.create({
    of: CompatData,
    path: `${__dirname}/../data/json/${CompatData.id}.json`
  }, container);

  const composeReleaseOrder = function() {
    let props = Array.from(arguments);
    let order = props.shift();
    for (const prop of props) {
      order = E.THEN_BY(order, prop);
    }
    return order;
  };
  const releasesSink = await releaseDAO.orderBy(composeReleaseOrder(
    pkg.Release.RELEASE_DATE,
    pkg.Release.BROWSER_NAME,
    pkg.Release.BROWSER_VERSION,
    pkg.Release.OS_NAME,
    pkg.Release.OS_VERSION)).select();
  const releases = releasesSink.array;

  const igen = pkg.InteropClassGenerator.create(null, container);
  const spec = igen.generateSpec('org.chromium.apis.web.generated', 'InteropData', releases);
  const InteropData = igen.generateClass(spec);
  const props = InteropData.getAxiomsByClass(pkg.InteropProperty);

  let apiMap = new Map();

  for (const prop of props) {
    const currentReleases = prop.releases;
    const releaseDate = prop.releaseDate;

    let cols = [];
    for (const release of currentReleases) {
      cols.push(pkg.generated.CompatData.getAxiomByName(gen.propertyNameFromRelease(release)));
    }

    for (let j = 0; j <= 4; j++) {
      const q = E.EQ(E.ARRAY_COUNT(E.SEQ.apply(E, cols), E.TRUTHY()), j);
      const sink = await compatDAO.where(q).select();
      const apis = sink.array;
      for (const api of apis) {
        if (!apiMap.get(api.id)) {
          apiMap.set(api.id, InteropData.create({
            interfaceName: api.interfaceName,
            apiName: api.apiName,
          }, container));
        }
        let interopData = apiMap.get(api.id);
        prop.set(interopData, j);
      }
    }
  }

  const apis = Array.from(apiMap.values());

  await Promise.all([
    storeClassModel(InteropData.model_),
    storeDataFromArray(apis, InteropData),
  ]);

  console.log('Stored model and data');

  // let currentReleasesMap = new Map();
  // let i;
  // for (i = 0; currentReleasesMap.size < 4; i++) {
  //   currentReleasesMap.set(releases[i].browserName, releases[i]);
  // }

  // for (i--; i < releases.length; i++) {
  //   currentReleasesMap.set(releases[i].browserName, releases[i]);
  //   const currentReleases = Array.from(currentReleasesMap.values());
  //   let cols = [];
  //   for (const release of currentReleases) {
  //     cols.push(CompatData.getAxiomByName(gen.propertyNameFromRelease(release)));
  //   }

  //   for (let j = 1; j <= 4; j++) {
  //     const q = E.EQ(E.ARRAY_COUNT(E.SEQ.apply(E, cols), E.TRUTHY()), j);
  //     const count = await compatDAO.where(q).select(E.COUNT());
  //     console.log(releases[i].releaseDate, j, count.value);
  //   }
  // }
})();

// const compatClassURL = `file://${__dirname}/../data/json/${pkg.DAOContainer.COMPAT_MODEL_FILE_NAME}`;
// pkg.ClassGenerator.create({
//   classURL: compatClassURL,
// }).generateClass().then(CompatData => {
//   container.releaseDAO = pkg.LocalJsonDAO.create({
//     of: pkg.Release,
//     path: `${__dirname}/../data/json/${pkg.Release.id}.json`
//   }, container);
//   container.compatDAO = pkg.LocalJsonDAO.create({
//     of: CompatData,
//     path: `${__dirname}/../data/json/${CompatData.id}.json`
//   }, container);
// });
