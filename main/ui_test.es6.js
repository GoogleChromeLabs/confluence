// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

//
// Setup environment for manual UI tests. "uiTestEnvPromise" resolves to object
// referring to UI view objects for use in manual tests.
//

// Refinements before models.
require('../lib/property.es6.js');

require('../lib/web_apis/release.es6.js');
require('../lib/web_apis/web_interface.es6.js');
require('../lib/web_apis/release_interface_relationship.es6.js');
require('../lib/web_apis/api_compat_data.es6.js');
require('../lib/web_apis/relational_to_compat.es6.js');
require('../lib/u2/ScrollDAOTable.es6.js');
const pkg = org.chromium.apis.web;

global.uiTestEnvPromise = (async function() {
  const releases = [
    pkg.Release.create({
      browserName: 'A',
      browserVersion: '1',
      osName: 'X',
      osVersion: '1',
    }),
    pkg.Release.create({
      browserName: 'B',
      browserVersion: '1',
      osName: 'X',
      osVersion: '1',
    }),
    pkg.Release.create({
      browserName: 'C',
      browserVersion: '1',
      osName: 'Y',
      osVersion: '1',
    }),
    pkg.Release.create({
      browserName: 'D',
      browserVersion: '1',
      osName: 'Z',
      osVersion: '1',
    }),
  ];
  let apis = [];
  for (let i = 0; i < 30; i++) {
    for (let j = 0; j < 10; j++) {
      apis.push(pkg.WebInterface.create({
        interfaceName: `Interface${i}`,
        apiName: `api${j}${j % 10 === 5 ?
                      'withAVeryLongNameThatOverflowsOutsideCell' : ''}`,
      }));
    }
  }
  let joins = [];
  for (let i = 0; i < releases.length; i++) {
    for (let j = 0; j < apis.length; j++) {
      if (j  % 2 == i % 2) {
        joins.push(pkg.ReleaseWebInterfaceJunction.create({
          sourceId: releases[i],
          targetId: apis[j],
        }));
      }
    }
  }

  const clsAndData = pkg.RelationalToCompatConverter.create().convert(
      releases, apis, joins);
  const Cls = clsAndData.cls;
  const array = clsAndData.data;

  const data = foam.dao.MDAO.create({
    of: Cls,
  });
  await Promise.all(array.map(item => data.put(item)));

  const columns = [Cls.ID].concat(Cls.getAxiomsByClass(pkg.CompatProperty));
  const removeColumn  = () => true;

  return {
    scrollDAOTableTestView: pkg.ScrollDAOTable
        .create({data}, foam.createSubContext({
          columns,
          removeColumn,
        })),
  };
})();
