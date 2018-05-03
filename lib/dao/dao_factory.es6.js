// Copyright 2018 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

foam.INTERFACE({
  name: 'DAOFactory',
  package: 'org.chromium.apis.web',

  methods: [
    {
      name: 'create',
      documentation: `Instance-level Class.create(opts, ctx) for DAO
          factories.`,
      args: [
        {
          name: 'opts',
          documentation: `Property-initialization options, as in
              Class.create().`,
        },
        {
          name: 'ctx',
          documentation: 'FOAM context object in which to instantiate objects.',
        }
      ],
      returns: 'foam.dao.DAO',
      code: function() {},
    },
  ],
})

foam.CLASS({
  name: 'RestDAOFactory',
  package: 'org.chromium.apis.web',
  implements: ['org.chromium.apis.web.DAOFactory'],

  requires: ['foam.dao.RestDAO'],

  properties: [
    {
      class: 'String',
      name: 'baseURL',
      documentation: 'Pre-bound foam.dao.RestDAO.baseURL value.',
      final: true,
      required: true,
    },
  ],

  methods: [
    function create(opts, ctx) {
      this.validate();

      return this.RestDAO.create(
          Object.assign({baseURL: this.baseURL}, opts), ctx);
    }
  ]
});
