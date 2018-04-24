// Copyright 2018 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

foam.CLASS({
  name: 'DAOController',
  package: 'org.chromium.apis.web',

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
  ],
});
