// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('./failure_to_ship_data.es6.js');

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'FailureToShip',
  extends: 'org.chromium.apis.web.MetricComputer',
  implements: ['org.chromium.mlang.Expressions'],

  documentation: `Metric computer for number of APIs that have been shipping
      in all other browsers for a one-year grace period, but have never been
      shipped in this browser.`,

  requires: [
    'foam.dao.AnonymousSink',
    'foam.dao.EasyDAO',
    'org.chromium.apis.web.FailureToShipData',
    'org.chromium.apis.web.Release',
  ],
  imports: ['releaseDAO'],

  properties: [
    {
      name: 'failureToShipDAO',
      documentation: `This is a DAO that contains failureToShipData.`,
      final: true,
      factory: function() {
        return this.EasyDAO.create({
          name: 'failureToShipDAO',
          of: this.FailureToShipData,
          daoType: 'MDAO',
        });
      },
    },
  ],
  methods: [
    {
      name: 'compute',
      documentation: `Compute failure to ship value for each release in
          releases at the given date.`,
      args: [
      ],
      code: function(releases, date) {
        let computeForReleases = [];
        for (let i = 0; i < releases.length; i++) {
          let release = releases[i];
          let year = date.getFullYear();
          let month = date.getMonth();
          let day = date.getDate();
          let previousYear = new Date(year - 1, month, day);
          let releaseKeys = releases.map((release) => release.releaseKey);
          // Find all releases that satisfy one of:
          //   - a previous version of the "release".
          //   - some other release but released in previous one year range.
          //   - if no release in one year, use most recent release.
          computeForReleases.push(this.releaseDAO.where(this.OR(
              this.IN(this.Release.RELEASE_KEY, releaseKeys),
              this.AND(
                  this.EQ(this.Release.BROWSER_NAME, release.browserName),
                  this.EQ(this.Release.OS_NAME, release.osName),
                  this.GT(this.Release.RELEASE_DATE, previousYear),
                  this.LT(this.Release.RELEASE_DATE, date)),
              this.AND(
                  this.GT(this.Release.RELEASE_DATE, previousYear),
                  this.LT(this.Release.RELEASE_DATE, date))
              )).select().then((result) => {
            // releaseIface are a map of interfaces from this release in
            // recent one year's release.
            let releaseIface = {};
            let numOtherReleases = 0;
            // majorIface are a map of interfaces of form:
            //     {interfaceKey: <number of appearances>}.
            // Interfaces with number of appearances equals to numOtherReleases
            // are considered as major interfaces.
            let majorIface = {};
            let promises = [];
            let prevReleases = [];
            let comparedReleases = [];
            for (let i = 0; i < result.a.length; i++) {
              if (result.a[i].browserName !== release.browserName) {
                numOtherReleases++;
                comparedReleases.push(result.a[i]);
              } else if (result.a[i].releaseKey !== release.releaseKey) {
                prevReleases.push(result.a[i]);
              }
              promises.push(result.a[i].interfaces.select(
                  this.AnonymousSink.create({sink: {put: function(iface) {
                    if (result.a[i].browserName === release.browserName) {
                      releaseIface[iface.interfaceKey] = true;
                    } else {
                      // Interfaces may have names defined in majorIface's
                      // prototype chain; that's why we don't use
                      // majorIface[iface.interfaceKey] =
                      //     (majorIface[iface.interfaceKey] || 0) + 1.
                      if (!majorIface.hasOwnProperty(iface.interfaceKey)) {
                        majorIface[iface.interfaceKey] = 0;
                      }
                      majorIface[iface.interfaceKey]++;
                    }
                  }}})));
            }
            return Promise.all(promises).then(() => {
              // Find interfaces that all other browsers' releases supports
              // but not available in any version of this browser's releases.
              let numFailureToShip = 0;
              for (let ifaceKey in majorIface) {
                if (!majorIface.hasOwnProperty(ifaceKey)) continue;
                if (majorIface[ifaceKey] !== numOtherReleases) continue;
                if (!releaseIface.hasOwnProperty(ifaceKey)) {
                  numFailureToShip++;
                }
              }
              return this.failureToShipDAO.put(this.FailureToShipData.create(
                {
                  browserName: release.browserName,
                  release,
                  prevReleases,
                  comparedReleases,
                  numFailureToShip,
                  date,
                }));
            });
          }));
        }
        return Promise.all(computeForReleases);
      },
    },
  ],
});
