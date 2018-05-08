// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('angular');

require('../compat.es6.js');
require('../component/catalog_table.es6.js');
require('../dao/dao_container.es6.js');
require('../u2/ActionView.es6.js');
require('../u2/DAOController.es6.js');
require('../u2/DAOControllerView.es6.js');
require('../u2/ScrollDAOTable.es6.js');
require('../u2/SearchView.es6.js');
require('../u2/ToggleListElementsView.es6.js');
require('../web_apis/api_compat_data.es6.js');
require('../web_apis/api_compat_data.es6.js');
require('../web_apis/relational_to_compat.es6.js');
const pkg = org.chromium.apis.web;

angular.module('confluence').controller('catalogController', ['api',
  function(api) {
    const compatClassURL = `${window.location.origin}/${pkg.DAOContainer.COMPAT_MODEL_FILE_NAME}`;
    let CompatData;
    pkg.ClassGenerator.create({
      classURL: compatClassURL,
    }).generateClass().then(Cls => {
      CompatData = Cls;

      const compatDAO = api.getApiServiceDAO(
          pkg.DAOContainer.COMPAT_NAME, CompatData);
      const unorderdProperties = 
          CompatData.getAxiomsByClass(pkg.CompatProperty);
      const propDAO = foam.dao.ArrayDAO.create({array: unorderdProperties});
      const E = foam.mlang.ExpressionsSingleton.create();
      const composeReleaseOrder = function() {
        let props = Array.from(arguments);
        let order = E.DOT(pkg.CompatProperty.RELEASE, props.shift());
        for (const prop of props) {
          order = E.THEN_BY(order, E.DOT(pkg.CompatProperty.RELEASE, prop));
        }
        return order;
      };

      const orderedDAO = propDAO.orderBy(composeReleaseOrder(
          pkg.Release.RELEASE_DATE,
          pkg.Release.BROWSER_NAME,
          pkg.Release.BROWSER_VERSION,
          pkg.Release.OS_NAME,
          pkg.Release.OS_VERSION));

      return Promise.all([
        orderedDAO.select(), 
        orderedDAO.select(E.GROUP_BY(
                E.DOT(pkg.CompatProperty.RELEASE, pkg.Release.BROWSER_NAME),
            foam.dao.ArraySink.create())),
      ]);
    }).then(arraySinkAndGroupSink => {
      const arraySink = arraySinkAndGroupSink[0];
      const groupSink = arraySinkAndGroupSink[1];

      const compatProperties = arraySink.array;
      const removeColumn  = () => true;
      const selectable = compatProperties;

      let selected = [];
      groupSink.groupKeys.sort();
      for (const browserName of groupSink.groupKeys) {
        const arraySink = groupSink.groups[browserName];
        const releases = arraySink.array;
        selected.push(releases[releases.length - 1]);
      }

      const ctx = api.boxCtx.__subContext__.createSubContext({
        selectable,
        selected,
        removeColumn,
      });
  
      const daoController = pkg.DAOController.create({
        data: api.getApiServiceDAO(
            pkg.DAOContainer.COMPAT_NAME, CompatData, ctx),
      }, ctx);
      const daoControllerView = pkg.DAOControllerView
          .create(null, daoController);
      
      const container = document.querySelector('#api_catalog');
      container.innerHTML = daoControllerView.outerHTML;
      daoControllerView.load();
    });
  }]);
