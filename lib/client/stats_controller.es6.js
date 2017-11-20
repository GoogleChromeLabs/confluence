// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'StatsController',

  documentation: `Controller for reporting website statistics.`,

  imports: [
    'info',
    'window?',
  ],

  topics: [
    'displayMatrix',
    'mergedDisplayMatrix',
  ],

  properties: [
    {
      class: 'Function',
      name: 'getTime',
      factory: function() {
        return foam.isServer ?
            require('process').hrtime.bind(require('process')) :
            this.window.performance.now.bind(this.window.performance);
      }
    },
    {
      class: 'Function',
      name: 'reportStat',
      factory: function() {
        return function(key, value) {
          this.info(key, value);
        };
      },
    },
    {
      class: 'String',
      name: 'initialHash',
      documentation: `URL hash part upon initialization, if applicable.`,
    },
  ],

  methods: [
    function init() {
      this.SUPER();

      // onDisplayMatrix dispatches 'displayMatrix' to merged listener:
      // onDisplayMatrix_.
      this.displayMatrix.sub(this.onDisplayMatrix_);
      // onDisplayMatrix_ dispatches 'mergedDisplayMatrix' to one-time listener:
      // initialDisplayMatrix.
      this.mergedDisplayMatrix.sub(foam.events.oneTime(
          this.initialDisplayMatrix));

      // Store initial location hash (if not injected). This signals some
      // listeners as to whether their data are meaningful (i.e., whether the
      // "right page" was loaded at time=0).
      if (!this.initialHash && this.window && this.window.location) {
        this.initialHash = this.window.location.hash;
      }
    },
  ],

  listeners: [
    function onDisplayMatrix(matrix) {
      const time = this.getTime();
      this.displayMatrix.pub(matrix, time);
    },
    {
      name: 'onDisplayMatrix_',
      documentation: `Debounce reporting display matrix for 10s.`,
      isMerged: true,
      mergeDelay: 10000,
      code: function(sub, topic, matrix, time) {
        this.mergedDisplayMatrix.pub(matrix, time);
      },
    },
    function initialDisplayMatrix(sub, topic, matrix, time) {
      if (!/^#!\/catalog/.test(this.initialHash)) return;
      if (Object.keys(matrix).length === 0) {
        this.reportStat('Initial matrix empty', time);
      } else {
        this.reportStat('Initial display matrix', time);
      }
    },
  ],
});
