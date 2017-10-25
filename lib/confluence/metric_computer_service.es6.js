// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('../web_apis/release.es6.js');
require('../web_apis/release_interface_relationship.es6.js');
require('../web_apis/web_interface.es6.js');
require('./aggressive_removal.es6.js');
require('./browser_metric_data.es6.js');
require('./metric_computer.es6.js');

foam.ENUM({
  name: 'MetricComputerType',
  package: 'org.chromium.apis.web',

  properties: [
    {
      class: 'Class',
      name: 'metricComputerCls',
    },
    {
      class: 'FObjectProperty',
      of: 'org.chromium.apis.web.MetricComputer',
      name: 'metricComputer',
      factory: function() {
        return this.metricComputerCls.create(null, this);
      },
    },
  ],

  values: [
    {
      name: 'AGGRESSIVE_REMOVAL',
      metricComputerCls: 'org.chromium.apis.web.AggressiveRemoval',
    },
    {
      name: 'BROWSER_SPECIFIC',
      metricComputerCls: 'org.chromium.apis.web.BrowserSpecific',
    },
    {
      name: 'FAILURE_TO_SHIP',
      metricComputerCls: 'org.chromium.apis.web.FailureToShip',
    },
  ],
});

foam.CLASS({
  name: 'MetricComputerService',
  package: 'org.chromium.apis.web',

  requires: [
    'org.chromium.apis.web.Release',
    'org.chromium.apis.web.ReleaseWebInterfaceJunction',
    'org.chromium.apis.web.WebInterface',
  ],
  exports: [
    'releaseDAO',
    'webInterfaceDAO',
    'realeaseWebInterfaceJunctionDAO',
    'browserMetricsDAO',
  ],

  properties: [
    {
      class: 'FObjectProperty',
      of: 'foam.mlang.predicate.Predicate',
      name: 'releasePredicate',
      required: true,
    },
    {
      class: 'foam.dao.DAOProperty',
      name: 'releaseDAO',
      transient: true,
      factory: function() {
        this.validate();
        // Ensure that releases are filtered according to service configuration.
        // This maintains consistency between releases passed in to "compute"
        // and releases available to seen by this service.
        return this.getDAO_(this.Release).where(this.releasePredicate);
      },
    },
    {
      class: 'foam.dao.DAOProperty',
      name: 'webInterfaceDAO',
      transient: true,
      factory: function() {
        return this.getDAO_(this.WebInterface);
      },
    },
    {
      class: 'foam.dao.DAOProperty',
      name: 'realeaseWebInterfaceJunctionDAO',
      transient: true,
      factory: function() {
        return this.getDAO_(this.ReleaseWebInterfaceJunction);
      },
    },
    {
      class: 'foam.dao.DAOProperty',
      name: 'browserMetricsDAO',
      transient: true,
      factory: function() {
        return this.MDAO.create({of: this.BrowserMetricData});
      },
    },
  ],

  methods: [
    {
      name: 'compute',
      returns: 'Promise',
      code: function(metricComputerType, releases, date) {
        debugger;
        // Compute metrics (which output to "browserMetricsDAO"), then return
        // array of computed metrics.
        return metricComputerType.metricComputer.compute(releases, date)
            .then(() => this.browserMetricsDAO.select())
            .then(arraySink => arraySink.array);
      },
    },
    function getDAO_(cls) {
      // Special indexing for Release <==> WebInterface junctions.
      const indicies = cls === this.ReleaseWebInterfaceJunction ?
            [
              ['sourceId', false],
              ['targetId', false],
            ] :
            [];

      // Load JSON in to ArrayDAO.
      const jsonDAO = this.LocalJsonDAO.create({
        of: cls,
        path: `${__dirname}/../../data/json/${cls.id}.json`,
      });
      // Create indexed MDAO for fast queries.
      const dao = this.MDAO.create({
        of: cls,
        indicies,
      });

      // Copy ArrayDAO => MDAO; return MDAO.
      //
      // Note: Use of DAOSink without further synchronization is sound because
      // "dao" is a *synchronous* DAO implementation.
      return jsonDAO.select(this.DAOSink.create({dao})).then(() => dao);
    },
  ],
});
