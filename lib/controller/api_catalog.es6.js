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
      const unorderedProperties = 
          CompatData.getAxiomsByClass(pkg.CompatProperty);
      const propDAO = foam.dao.ArrayDAO.create({array: unorderedProperties});
      const E = foam.mlang.ExpressionsSingleton.create();
      const composeReleaseOrder = function() {
        let props = Array.from(arguments);
        let order = E.DOT(pkg.CompatProperty.RELEASE, props.shift());
        for (const prop of props) {
          order = E.THEN_BY(order, E.DOT(pkg.CompatProperty.RELEASE, prop));
        }
        return order;
      };

      return propDAO.orderBy(composeReleaseOrder(
              pkg.Release.RELEASE_DATE,
              pkg.Release.BROWSER_NAME,
              pkg.Release.BROWSER_VERSION,
              pkg.Release.OS_NAME,
              pkg.Release.OS_VERSION))
          .select();
    }).then(arraySink => {
      const compatProperties = arraySink.array;
      const selectable = compatProperties;

      // Get latest release of each browser by storing first of each found in
      // reversed array.
      const rSelectable = Array.from(selectable).reverse();
      let selectedMap = new Map();
      let rSelected = [];
      for (const prop of rSelectable) {
        const browserName = prop.release.browserName;
        if (!selectedMap.has(browserName)) {
          selectedMap.set(browserName, true);
          rSelected.push(prop);
        }
      }
      // Undo reverse ordering.
      const selected = rSelected.reverse();

      // Bind to URL state.
      const state = pkg.CatalogState.create({
        selected,
        urlState: api.urlState,
      }, api.boxCtx.__subContext__.createSubContext({
        selectable,
      }));
      const ctx = state.__subContext__;
  
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
