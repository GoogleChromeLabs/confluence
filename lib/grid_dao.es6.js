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
  package: 'org.chromium.apis.web',
  name: 'GridDAO',
  extends: 'foam.dao.ProxyDAO',

  documentation: 'DAO of GridRows that stores GridRow.data metadata.',

  properties: [
    {
      name: 'of',
      value: 'org.chromium.apis.web.GridRow',
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
  ],
});
