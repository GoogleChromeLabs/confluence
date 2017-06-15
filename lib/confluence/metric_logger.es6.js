// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'MetricLogger',

  imports: ['info'],

  properties: [
    {
      class: 'String',
      name: 'name',
    },
  ],

  methods: [
    function startRun() {
      this.info(this.name, 'running');
    },
    function startGatherReleases() {
      this.info(this.name, 'gathering releases');
    },
    function startCompute(date, release) {
      this.logLines_(
          [this.name, 'starting compute'],
          ['Date', date],
          ['Release', release.id]);
    },
    function startComputeForReleases(date, release, compared, previous) {
      this.logLines_(
          [this.name, 'computing for releases'],
          ['Date', date],
          ['Release', release.id],
          ['Compared releases'].concat(this.ids_(compared)),
          ['Previous releases'].concat(this.ids_(previous)));
    },
    function finishComputeForReleases(
        date, release, compared, previous, value) {
      this.logLines_(
          [this.name, 'computing for releases'],
          ['Date', date],
          ['Release', release.id],
          ['Compared releases'].concat(this.ids_(compared)),
          ['Previous releases'].concat(this.ids_(previous)),
          ['Value', value]);
    },
    function ids_(releases) { return releases.map(this.id_); },
    function id_(release) { return release.id; },
    function logLines_() {
      // Log multiple lines, passing individual objects to the logging function,
      // along with line separators.
      var args = [];
      for (var i = 0; i < arguments.length; i++) {
        args = args.concat(arguments[i]);
        if (i !== arguments.length - 1) args.push('\n   ');
      }
      this.info.apply(this, args);
    },
  ],
});
