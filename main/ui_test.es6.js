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

require('../lib/u2/ActionView.es6.js');
require('../lib/u2/DAOController.es6.js');
require('../lib/u2/DAOControllerView.es6.js');
require('../lib/u2/ScrollDAOTable.es6.js');
require('../lib/u2/SearchView.es6.js');
require('../lib/u2/ToggleListElementsView.es6.js');
require('../lib/web_apis/api_compat_data.es6.js');
require('../lib/web_apis/relational_to_compat.es6.js');
require('../lib/web_apis/release_interface_relationship.es6.js');
require('../lib/web_apis/release.es6.js');
require('../lib/web_apis/web_interface.es6.js');
const pkg = org.chromium.apis.web;

global.uiTestEnvPromise = (function() {
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
  return Promise.all(array.map(item => data.put(item))).then(() => {
    foam.CLASS({
      name: 'SearchViewWrapper',
      package: 'org.chromium.test',
      extends: 'foam.u2.View',

      imports: [
        'selectable as importedSelectable',
        'selected as importedSelected',
      ],
      exports: [
        'error',
        'predicate',
      ],

      properties: [
        {
          class: 'String',
          name: 'data',
        },
        {
          name: 'predicate',
          value: null,
          postSet: function(old, nu) {
            // Clear error message on new successful parse.
            if (nu && old !== nu) this.errorMessage = '';
          },
        },
        {
          class: 'foam.u2.ViewSpec',
          name: 'searchViewSpec',
          value: {class: 'org.chromium.apis.web.SearchView'},
        },
        {
          name: 'searchView',
        },
        {
          class: 'Function',
          name: 'error',
        },
        {
          class: 'String',
          name: 'errorMessage',
        },
        {
          class: 'Array',
          of: 'String',
          name: 'selectable',
        },
        {
          class: 'Array',
          of: 'String',
          name: 'selected',
        },
      ],

      methods: [
        function init() {
          this.SUPER();
          this.error = function() {
            const args = Array.from(arguments);
            const msg = args.map(arg => arg.message || arg).join(' ');
            if (!msg) return;
            this.errorMessage = msg;
          }.bind(this);
          this.importedSelectable$
            .mapTo(this.selectable$, function(array) {
              return array.map(prop => prop.release.id);
            });
          this.importedSelected$
            .mapTo(this.selected$, function(array) {
              return array.map(prop => prop.release.id);
            });
        },
        function initE() {
          this.addClass(this.myClass())
            .start('div')
                .add('Selectable (browserName_browserVersion_osName_osVersion): ')
                .add(this.selectable$.map(selectable => selectable.join(', ')))
            .end().start('div')
                .add('Selected (browserName_browserVersion_osName_osVersion): ')
                .add(this.selected$.map(selected => selected.join(', ')))
            .end().start('br').end()
            .start('div').style({border: '1px solid black'})
                .tag(this.searchViewSpec, {data$: this.data$}, this.searchView$)
            .end().start('br').end()
            .start('div')
                .add('Predicate:').start('br').end()
                .start('pre')
                    .add(this.predicate$
                        .map(predicate => predicate ? predicate.toString() : ''))
                .end()
            .end().start('div')
                .add('Error:').start('br').end()
                .start('pre')
                    .add(this.errorMessage$)
                .end()
            .end();
          this.SUPER();
        },
      ],
    });

    const compatProperties = Cls.getAxiomsByClass(pkg.CompatProperty);
    const columns = [Cls.ID].concat(compatProperties);
    const removeColumn  = () => true;
    const selectable = compatProperties;
    const selected = compatProperties.slice(-2);
    const ctx = foam.createSubContext({
      selectable,
      selected,
      columns,
      removeColumn,
    });

    const daoController = pkg.DAOController.create({data}, ctx);

    return {
      context: ctx,
      scrollDAOTableTestView: pkg.ScrollDAOTable.create({data}, ctx),
      searchViewTestView: org.chromium.test.SearchViewWrapper.create(null, ctx),
      daoControllerViewTestView: pkg.DAOControllerView
          .create(null, daoController),
    };
  });
})();
