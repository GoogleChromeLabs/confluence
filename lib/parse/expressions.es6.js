// Copyright 2018 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

foam.CLASS({
  name: 'Seq',
  package: 'org.chromium.mlang',
  extends: 'foam.mlang.predicate.Nary',

  documentation: 'Apply a sequence of functions over input.',

  properties: [
    {
      name: 'f',
      value: function f(o) {
        return this.args.map(arg => arg.f(o));
      },
    },
  ],
});

foam.CLASS({
  name: 'ParseExpressions',
  package: 'org.chromium.mlang',
  refines: 'foam.mlang.Expressions',

  requires: ['org.chromium.mlang.Seq'],

  methods: [
    function SEQ() {
      return this.Seq.create({args: Array.from(arguments)});
    },
  ],
});
