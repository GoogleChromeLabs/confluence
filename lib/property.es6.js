// Copyright 2018 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

foam.CLASS({
  name: 'Property',
  package: 'org.chromium.apis.web',
  refines: 'foam.core.Property',

  documentation: `Refine properties to contain a rawTableCellFormatter()
      function that can be leveraged by foam.u2.RowFormatters to delegate
      outputting individual table cells.`,

  properties: [
    {
      class: 'Function',
      name: 'rawTableCellFormatter',
      value: null,
    },
    {
      class: 'String',
      name: 'gridTemplateColumn',
      value: '1fr',
    },
  ],
});
