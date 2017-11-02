// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('../web_apis/release.es6.js');

foam.CLASS({
  name: 'MetricComputer',
  package: 'org.chromium.apis.web',
  implements: ['foam.mlang.Expressions'],

  documentation: `An abstract class for computing an historical browser metric
      over all release dates for all known browser releases. The run() method
      iterates over all browser release dates and invokes compute() to store
      <number of browsers> metric values. The compute() method stores these
      values given:

      (1) The latest release of each browser as of a particular date;
      (2) The date for which the metric.`,

  requires: [
    'org.chromium.apis.web.Release',
    'foam.dao.ArraySink',
  ],
  imports: [
    'releaseDAO as ctxReleaseDAO',
  ],

  properties: [
    {
      class: 'foam.dao.DAOProperty',
      name: 'releaseDAO',
      documentation: `DAO of releases used to compute metric. By default, this
          is desktop (i.e., non-mobile) releases in the DAO provided by the
          context.`,
      factory: function() {
        return this.ctxReleaseDAO.where(this.EQ(this.Release.IS_MOBILE, false));
      },
    },
  ],
});
