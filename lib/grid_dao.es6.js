// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'GridRow',

  properties: [
    {
      name: 'id',
    },
    {
      class: 'Array',
      documentation: 'Data items parallel to GridDAO.cols.',
      name: 'data',
    },
  ],
});


foam.CLASS({
  package: 'org.chromium.mlang.sink',
  name: 'Projection',
  extends: 'foam.mlang.ExprProperty',

  documentation: `Projection over particular data columns. Primarliy used to
      implement GridDAO.projectCols().

      E.g., myGridDAO.select(MAP(myGridDAO.projectCols(
                myGridDAO.cols[10],
                myGridDAO.cols[7],
                myGridDAO.cols[45])))`,

  properties: [
    {
      class: 'Array',
      of: 'Int',
      name: 'cols',
    },
    {
      name: 'f',
      value: function f(gridRow) {
        let row = gridRow.clone();
        let allData = row.data;
        const cols = this.cols;
        let data = new Array(cols.length);
        for (let i = 0; i < cols.length; i++) {
          data[i] = allData[cols[i]];
        }
        row.data = data;
        return row;
      }
    },
  ],
});

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'GridDAO',
  extends: 'foam.dao.ProxyDAO',

  documentation: 'DAO of GridRows that stores GridRow.data metadata.',

  requires: [
    'foam.dao.MDAO',
    'org.chromium.mlang.sink.Projection'
  ],
  imports: ['warn'],

  properties: [
    {
      name: 'of',
      value: 'org.chromium.apis.web.GridRow',
    },
    {
      name: 'delegate',
      factory: function() {
        return this.MDAO.create({of: this.of});
      },
    },
    {
      class: 'Array',
      documentation: 'Column metadata items parallel to GridRow.data.',
      name: 'cols',
      final: true,
      required: true,
    },
  ],

  methods: [
    {
      name: 'colIndexOf',
      documentation: `Return the array index (in "cols") of "col". If "col" does
          not appear in "cols", then return -1. Comparisons performed using
          "FOAM equality": foam.util.compare().`,
      code: function(col) {
        const cols = this.cols;
        for (let i = 0; i < cols.length; i++) {
          if (foam.util.equals(cols[i], col)) return i;
        }
        return -1;
      },
    },
    {
      name: 'projectCols',
      documentation: `Return a foam.mlang.ExprProperty that transforms a GridRow
          into a modified GridRow with data set to the list of columns described
          by arguments. Drop/ignore arguments that do not match any member of
          "cols".`,
      code: function() {
        const args = Array.from(arguments);
        const idxs = args.map(this.colIndexOf.bind(this));
        for (let i = 0; i < idxs.length; i++) {
          if (idxs[i] === -1) {
            this.warn('Ignoring unknown GridDAO column:', args[i]);
          }
        }
        const cols = idxs.filter(idx => idx !== -1);
        return this.Projection.create({cols});
      },
    },
  ],
});
