// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

foam.ENUM({
  name: 'BrowserMetricDataType',
  package: 'org.chromium.apis.web',

  properties: [
    {
      class: 'String',
      name: 'name',
      documentation: 'Constant name used to refer to this enum value.',
      required: true,
    },
    {
      class: 'String',
      name: 'label',
      documentation: 'Human-readable name describing this enum value.',
      required: true,
    },
    {
      class: 'String',
      documentation: 'Description of the metric and how it is computed.',
      name: 'description',
      required: true,
    },
    {
      class: 'String',
      documentation: `Description of metric "release", how it is selected
          relative to metric "releaseDate", and how it is used for metric
          computation.`,
      name: 'releaseDescription',
      required: true,
    },
    {
      class: 'String',
      documentation: `Description of metric "prevReleases", how they are
          selected relative to metric "releaseDate", and how it is used for
          metric computation.`,
      name: 'prevReleasesDescription',
      required: true,
    },
    {
      class: 'String',
      documentation: `Description of metric "comparedReleases", how they are
          selected relative to metric "releaseDate", and how it is used for
          metric computation.`,
      name: 'comparedReleasesDescription',
      required: true,
    },
  ],

  values: [
    {
      name: 'AGGRESSIVE_REMOVAL',
      label: 'Aggressive removal',
      description: `Count of APIs removed from a browser that are still
          part of every other browser's API surface. Count is computed with
          a one-year grace period for other browsers to also remove the API
          before it counts against the browser that was first to remove.`,
      releaseDescription: `The newest release of this browser that was released
          at prior to the the one-year grace period preceding this data point's
          "date".`,
      prevReleasesDescription: `Releases of this browser released prior to data
          point's "release".`,
      comparedReleasesDescription: `Current releases of other browsers (not this
          data point's "release" browser), as of "date".`,
    },
    {
      name: 'BROWSER_SPECIFIC',
      label: 'Browser-specific',
      description: `Count of APIs that:

          (1) Have been shipping in this browser for the duration of a one-year
              grace period,
          and
          (2) Have not shipped in any other browser at any point during the
              one-year grace period.`,
      releaseDescription: `The latest release of this browser as of data point's
          "date".`,
      prevReleasesDescription: `Releases of this browser released prior to data
          point's "release".`,
      comparedReleasesDescription: `Releases of other browsers (not data point's
          "release" browser) released during the one-year grace period prior to
          data point's "date".`,
    },
    {
      name: 'FAILURE_TO_SHIP',
      label: 'Failure to ship',
      description: `Number of APIs that have been shipping in all other browsers
          for a one-year grace period, but have never been shipped in this
          browser.`,
      releaseDescription: `The latest release of this browser as of data point's
          "date".`,
      prevReleasesDescription: `Releases of this browser released prior to data
          point's "release".`,
      comparedReleasesDescription: `Releases of other browsers (not data point's
          "release" browser) released during the one-year grace period prior to
          data point's "date".`,
    },
  ],
});

foam.CLASS({
  name: 'BrowserMetricValue',
  package: 'org.chromium.apis.web',
  extends: 'foam.core.Int',

  properties: [
    {
      name: 'value',
      documentation: `Negative default value triggers validateInstance() failure
          when no value is injected.`,
      value: -1,
    },
  ],

  // TODO(markdittmer): This custom implementation should be unnecessary if/when
  // default validateInstance() implementation checks against default value.
  methods: [
    function validateInstance(o) {
      if (o[this.name] < 0) throw 'Browser metric value must be non-negative';
    },
  ],
});

foam.CLASS({
  name: 'BrowserMetricData',
  package: 'org.chromium.apis.web',

  documentation: `Data associated with an historical browser metric data
      point.`,

  ids: ['type', 'browserName', 'date'],

  properties: [
    {
      class: 'Enum',
      of: 'org.chromium.apis.web.BrowserMetricDataType',
      name: 'type',
      required: true,
    },
    {
      class: 'String',
      name: 'browserName',
      documentation: 'Browser name associated with count.',
      required: true,
      final: true,
    },
    {
      class: 'Date',
      name: 'date',
      documentation: 'The date for which this metric is computed.',
      required: true,
      final: true,
    },
    {
      class: 'FObjectProperty',
      of: 'org.chromium.apis.web.Release',
      name: 'release',
      documentation: 'The primary release associated with this data point.',
      value: null,
      final: true,
    },
    {
      class: 'FObjectArray',
      of: 'org.chromium.apis.web.Release',
      name: 'prevReleases',
      documentation: `Previous releases from "browserName" (also from the same
          platform) used for metric computation.`,
      required: true,
      final: true,
    },
    {
      class: 'FObjectArray',
      name: 'comparedReleases',
      of: 'org.chromium.apis.web.Release',
      documentation: `Releases of other browsers (not "browserName") used for
          metric computation.`,
      required: true,
      final: true,
    },
    {
      class: 'org.chromium.apis.web.BrowserMetricValue',
      name: 'value',
      documentation: 'The value (API count) associated with this data point.',
      required: true,
      final: true,
    },
  ],
});

// Initialized Versioned form of class for SyncDAOs.
foam.version.VersionedClassFactorySingleton.create()
    .get(org.chromium.apis.web.BrowserMetricData);
