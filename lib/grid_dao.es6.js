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
  package: 'org.chromium.mlang',
  name: 'Projection',
  extends: 'foam.mlang.ExprProperty',

  documentation: `Projection over particular data columns that retains GridRow
      state. I.e., f() implementation is GridRow => GridRow, but modifies
      GridRow.data. Primarliy used to implement GridDAO.projectCols().

      E.g., myGridDAO.select(MAP(myGridDAO.projectCols(
                myGridDAO.cols[10],
                myGridDAO.cols[7],
                myGridDAO.cols[45])))`,

  requires: ['org.chromium.apis.web.GridRow'],

  properties: [
    {
      class: 'Array',
      of: 'Int',
      name: 'cols',
    },
    {
      name: 'f',
      value: function f(gridRow) {
        let row = this.GridRow.create({
          id: gridRow.id && gridRow.id.clone ? gridRow.id.clone(this) :
              gridRow.id,
        });
        let allData = gridRow.data;
        const cols = this.cols;
        let data = new Array(cols.length);
        for (let i = 0; i < cols.length; i++) {
          data[i] = allData[cols[i]];
        }
        row.data = data;
        return row;
      },
    },
  ],
});

foam.CLASS({
  package: 'org.chromium.mlang',
  name: 'Truthy',
  extends: 'foam.mlang.ExprProperty',

  axioms: [foam.pattern.Singleton.create()],

  properties: [{name: 'f', value: function(v) { return !!v; }}],
  methods: [function toString() { return 'isTruthy'; }],
});

foam.CLASS({
  package: 'org.chromium.mlang',
  name: 'ArrayExtract',
  extends: 'foam.mlang.predicate.Nary',

  documentation: `Predicate that extract elements of an array. Similar to
      Projection, but f() implementation is
      GridRow => extractThisDotArgs(GridRow.data). Primarily used to implement
      GridDAO.extractCols().

      E.g., myGridDAO.where(EQ(myGridDAO.extractCols(
                myGridDAO.cols[10],
                myGridDAO.cols[7],
                myGridDAO.cols[45]), [true, false, true]))`,

  methods: [
    function f(array) {
      const offsets = this.args;
      let extracted = new Array(offsets.length);
      for (let i = 0; i < offsets.length; i++) {
        extracted[i] = array[offsets[i]];
      }
      return extracted;
    },
  ],
});

foam.CLASS({
  package: 'org.chromium.mlang',
  name: 'ArrayCount',
  extends: 'foam.mlang.predicate.Binary',

  documentation: `Predicate that counts number of array elements from "arg1"
      returns truthy when applied to "arg2".

      E.g., myGridDAO.where(EQ(ARRAY_COUNT(
                myGridDAO.extractCols(
                    myGridDAO.cols[10],
                    myGridDAO.cols[7],
                    myGridDAO.cols[45]),
                TRUTHY()), 1))`,

  methods: [
    function f(o) {
      const array = this.arg1.f(o);
      let count = 0;
      for (const element of array) {
        if (this.arg2.f(element)) count++;
      }
      return count;
    },
  ],
});

foam.CLASS({
  package: 'org.chromium.mlang',
  name: 'GridExpressions',
  refines: 'foam.mlang.Expressions',

  requires: [
    'org.chromium.mlang.ArrayCount',
    'org.chromium.mlang.ArrayExtract',
    'org.chromium.mlang.Truthy',
  ],

  methods: [
    function TRUTHY() { return this.Truthy.create(); },
    function ARRAY_EXTRACT() {
      return this.ArrayExtract.create({
        args: Array.from(arguments),
      });
    },
    function ARRAY_COUNT(arrayMLang, predicatMLang) {
      return this.ArrayCount.create({arg1: arrayMLang, arg2: predicatMLang});
    },
  ],
});

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'GridDAO',
  extends: 'foam.dao.ProxyDAO',
  implements: ['org.chromium.mlang.GridExpressions'],

  documentation: 'DAO of GridRows that stores GridRow.data metadata.',

  requires: [
    'foam.dao.MDAO',
    'org.chromium.apis.web.GridRow',
    'org.chromium.mlang.Projection',
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
        const cols = this.getColIdxs_(Array.from(arguments));
        return this.Projection.create({cols});
      },
    },
    {
      name: 'extractCols',
      documentation: `Return a predicate that extracts from a GridRow the
          columns in its "data" described by arguments. Drop/ignore arguments
          that do not match any member of "cols".`,
      code: function() {
        const cols = this.getColIdxs_(Array.from(arguments));
        return this.DOT(this.GridRow.DATA, this.ArrayExtract.create({
          args: cols,
        }));
      },
    },
    function getColIdxs_(cols) {
      const idxs = cols.map(this.colIndexOf.bind(this));
      for (let i = 0; i < idxs.length; i++) {
        if (idxs[i] === -1) {
          this.warn('Ignoring unknown GridDAO column:', cols[i]);
        }
      }
      return idxs.filter(idx => idx !== -1);
    },
  ],
});
