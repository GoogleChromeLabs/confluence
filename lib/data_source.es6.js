// Copyright 2018 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

foam.ENUM({
  name: 'DataSource',
  package: 'org.chromium.apis.web',

  documentation: `Indicator for what "mode" of data sourcing to use.`,

  values: [
    {
      name: 'LOCAL',
      documentation: 'JSON data to be loaded from the local filesystem.',
    },
    {
      name: 'HTTP',
      documentation: 'JSON data to be loaded via HTTP GET.',
    },
  ],
});
