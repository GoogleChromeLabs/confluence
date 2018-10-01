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

// Analyze data for non-mobile releases.
const E = foam.mlang.ExpressionsSingleton.create();
const logger = foam.log.ConsoleLogger.create(null, foam.createSubContext({
  releasePredicate: E.EQ(pkg.Release.IS_MOBILE, false),
}));

let container = pkg.DAOContainer.create(null, logger);

(async () => {
  const classURL = `file://${__dirname}/../data/json/class:org.chromium.apis.web.generated.InteropData.json`;
  const InteropData = await pkg.ClassGenerator.create({
    classURL,
  }, container).generateClass();

  const interopDAO = pkg.LocalJsonDAO.create({
    of: InteropData,
    path: `${__dirname}/../data/json/${InteropData.id}.json`
  }, container);

  const props = InteropData.getAxiomsByClass(pkg.InteropProperty);
  let hdr = ['"API"'].concat(props.map(prop => {
    return `"${prop.releaseDate.toDateString()} : ${prop.releases.map(release => release.id).join(', ')}"`;
  }));
  let rows = [hdr];

  const sink = await interopDAO.orderBy(InteropData.ID).select();
  const array = sink.array;
  for (const api of array) {
    let row = [`"${api.id}"`];
    for (const prop of props) {
      row.push(`"${prop.get(api)}"`);
    }
    rows.push(row);
  }
  rows.push([]);
  for (let i = 0; i <= 4; i++) {
    let row = [`"COUNT(num_browsers=${i})"`];
    for (const prop of props) {
      const sink = await interopDAO.where(E.EQ(prop, i)).select(E.COUNT());
      const count = sink.value;
      row.push(`"${count}"`);
    }
    rows.push(row);
  }

  const report = rows.map(row => row.join(',')).join('\n');
  fs.writeFileSync(`${__dirname}/../data/interop.csv`, report);

  console.log('Wrote analysis results');
})();
