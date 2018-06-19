// Copyright 2018 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

foam.CLASS({
  name: 'DAOController',
  package: 'org.chromium.apis.web',
  implements: ['foam.mlang.Expressions'],

  exports: [
    'as data',
    'predicate',
  ],

  properties: [
    {
      class: 'foam.dao.DAOProperty',
      name: 'data',
      documentation: 'The DAO being controlled by this controller.',
      hidden: true,
    },
    {
      class: 'FObjectProperty',
      of: 'foam.mlang.predicate.Predicate',
      name: 'predicate',
      documentation: 'The predicate for filtering the underlying DAO.',
      hidden: true,
      value: null,
    },
    {
      name: 'filteredDAO',
      documentation: 'The underlying DAO filtered by the current predicate.',
      view: {class: 'org.chromium.apis.web.ScrollDAOTable'},
      expression: function(data, predicate) {
        return predicate ? data.where(predicate) : data;
      },
    },
    {
      class: 'Boolean',
      documentation: 'Indicator for in-progress downloadData operation.',
      name: 'downloadInProgress',
    },
  ],

  methods: [
    function downloadData(ctx) {
      this.downloadInProgress = true;
      const props = Array.from(ctx.selected);
      const q = this.predicate && !this.TRUE.equals(this.predicate) ?
          this.predicate.toString() : '';
      const dao = this.filteredDAO;

      return dao.select().then(arraySink => {
        const data = arraySink.array;
        const id = props.map(p => p.label)
            .concat(q ? [q] : []).join(',');
        let hash = 19;
        for (let i = 0; i < id.length; i++) {
          hash = ((hash << 5) - hash) + id.charCodeAt(i);
        }

        this.downloadInProgress = false;

        return {hash, data};
      }, err => {
        this.downloadInProgress = false;
        throw err;
      });
    },
  ]
});
