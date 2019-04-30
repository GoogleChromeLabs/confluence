// Copyright 2018 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

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
  name: 'Truthy',

  axioms: [foam.pattern.Singleton.create()],

  properties: [
    {
      name: 'f',
      value: function(v) {
        return !!v;
      },
    },
  ],

  methods: [function toString() {
    return 'TRUTHY';
  }],
});

foam.CLASS({
  name: 'Seq',
  package: 'org.chromium.mlang',
  extends: 'foam.mlang.predicate.Nary',

  documentation: 'Apply a sequence of functions over input.',

  properties: [
    {
      name: 'f',
      value: function f(o) {
        return this.args.map((arg) => arg.f(o));
      },
    },
  ],
});

foam.CLASS({
  name: 'ParseExpressions',
  package: 'org.chromium.mlang',
  refines: 'foam.mlang.Expressions',

  requires: [
    'org.chromium.mlang.ArrayCount',
    'org.chromium.mlang.Seq',
    'org.chromium.mlang.Truthy',
  ],

  methods: [
    function ARRAY_COUNT(arrayMLang, predicatMLang) {
      return this.ArrayCount.create({arg1: arrayMLang, arg2: predicatMLang});
    },
    function SEQ() {
      return this.Seq.create({args: Array.from(arguments)});
    },
    function TRUTHY() {
      return this.Truthy.create();
    },
  ],
});
