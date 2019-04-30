// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('../web_apis/api_compat_data.es6.js');
require('../web_apis/release.es6.js');
require('./api_count_data.es6');

foam.CLASS({
  name: 'ApiCount',
  package: 'org.chromium.apis.web',
  implements: ['foam.mlang.Expressions'],

  documentation: `Metric computer for API counts that Nth release to N-1th
      release of a browser.

      Counts are: {<total APIs>, <removed APIs>, <added APIs>}.`,

  requires: [
    'foam.dao.ArraySink',
    'foam.dao.EasyDAO',
    'org.chromium.apis.web.ApiCountData',
    'org.chromium.apis.web.generated.CompatData',
    'org.chromium.apis.web.CompatProperty',
    'org.chromium.apis.web.Release',
  ],
  imports: [
    'apiCountDAO',
    'compatDAO',
    'info',
  ],

  methods: [
    {
      name: 'compute',
      documentation: `Compute metrics for a group of releases from a particular
          browser/platform.`,
      code: function(releases, date) {
        const numReleases = releases.length;
        foam.assert(numReleases === 1 || numReleases === 2,
            'ApiCount.compute(): Expected 1 or 2 releases');
        const lastReleaseDate = releases[releases.length - 1].releaseDate;
        foam.assert(
            date.getTime() === lastReleaseDate.getTime(),
            `ApiCount.compute(): Expected date to match last release date:
                Date: ${date}
                Last release date: ${lastReleaseDate}`);

        if (numReleases === 1) {
          return this.computeForSingleRelease_(releases[0]);
        } else {
          return this.computeForPair_(releases[0], releases[1]);
        }
      },
    },
    {
      name: 'computeForSingleRelease_',
      documentation: `Compute single metric value for first release in a
          sequence.`,
      code: function(release) {
        this.info(`Computing ApiCount for single release ${release.id}`);
        const prop = this.CompatData.getAxiomsByClass(this.CompatProperty)
            .find((p) => p.release.equals(release));
        return this.compatDAO.where(this.EQ(prop, true)).select(this.COUNT())
            .then((count) => {
              this.info(`Computed ApiCount for single release ${release.id}`);
              return this.apiCountDAO.put(this.ApiCountData.create({
                releaseDate: release.releaseDate,
                browserName: release.browserName,
                currRelease: release,
                totalApis: count.value,
              }));
            });
      },
    },
    {
      name: 'computeForPair_',
      documentation: `Compute single metric value for "next" release, based on
          "prev" as previous browser release.`,
      code: function(prev, next) {
        this.info(`Computing ApiCount for release pair ${prev.id}, ${next.id}`);
        const props = this.CompatData.getAxiomsByClass(this.CompatProperty);
        const prevProp = props.find((p) => p.release.equals(prev));
        const nextProp = props.find((p) => p.release.equals(next));
        let totalApis = 0;
        let removedApis = 0;
        let newApis = 0;
        return this.compatDAO.where(this.OR(
            this.AND(
                this.EQ(prevProp, true),
                this.EQ(nextProp, false)),
            this.EQ(nextProp, true))).select().then((arraySink) => {
          const apis = arraySink.array;
          for (const api of apis) {
            if (!nextProp.f(api)) removedApis++;
            else totalApis++;
            if (!prevProp.f(api)) newApis++;
          }

          this.info(`Computed ApiCount for release pair ${prev.id}, ${next.id}`);
          return this.apiCountDAO.put(this.ApiCountData.create({
            releaseDate: next.releaseDate,
            browserName: next.browserName,
            currRelease: next,
            prevRelease: prev,
            totalApis,
            removedApis,
            newApis,
          }));
        });
      },
    },
  ],
});
