// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('./browser_specific_data.es6.js');

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'BrowserSpecific',
  extends: 'org.chromium.apis.web.MetricComputer',
  implements: ['org.chromium.mlang.Expressions'],

  documentation: `Metric computer for count of APIs that:

      (1) Have been shipping in this browser for the duration of a one-year
          grace period,
      and
      (2) Have not shipped in any other browser at any point during the
          one-year grace period.`,
  requires: [
    'foam.dao.AnonymousSink',
    'foam.dao.EasyDAO',
    'org.chromium.apis.web.BrowserSpecificData',
    'org.chromium.apis.web.Release',
  ],
  imports: ['releaseDAO'],

  properties: [
    {
      name: 'browserSpecificDAO',
      documentation: `This is a DAO that contains browser specific data.`,
      final: true,
      factory: function() {
        return this.EasyDAO.create({
          name: 'browserSpecificDAO',
          of: this.BrowserSpecificData,
          daoType: 'MDAO',
        });
      },
    },
  ],

  methods: [
    {
      name: 'compute',
      documentation: `Compute browser-specific API value for "date", given
          latest release from each browser as of "date": "releases".`,
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
          //   - a previous version of the "release" browser.
          //   - a release during the one-year grace period from some other
          //     browser (not "release"'s).
          //   - if no release in one year from some browser, use most recent
          //     release.
          computeForReleases.push(this.releaseDAO.where(this.OR(
              this.AND(
                  this.IN(this.Release.RELEASE_KEY, releaseKeys)),
              this.AND(
                  this.EQ(this.Release.BROWSER_NAME, release.browserName),
                  this.EQ(this.Release.OS_NAME, release.osName),
                  this.GT(this.Release.RELEASE_DATE, previousYear),
                  this.LT(this.Release.RELEASE_DATE, date)),
              this.AND(
                  this.GT(this.Release.RELEASE_DATE, previousYear),
                  this.LT(this.Release.RELEASE_DATE, date))))
              .select().then((result) => {
                // releaseIface are a map of the form:
                //   {interfaceKey: <number of appearance in preview releases.>}
                // Interface with number of appearance equals to numPrevReleases
                // are considered as interface never been removed.
                let releaseIface = {};
                let numPrevReleases = 1;
                // allOtherIfaces are a map of interfaceKey that appears
                // in other browsers' release.
                let allOtherIfaces = {};
                let promises = [];
                let prevReleases = [];
                let comparedReleases = [];
                for (let i = 0; i < result.a.length; i++) {
                  if (result.a[i].browserName !== release.browserName) {
                    comparedReleases.push(result.a[i]);
                  } else if (result.a[i].releaseKey !== release.releaseKey) {
                    numPrevReleases++;
                    prevReleases.push(result.a[i]);
                  }
                  promises.push(result.a[i].interfaces.dao.select(
                      this.AnonymousSink.create({sink: {
                        put: function(iface) {
                          if (result.a[i].browserName === release.browserName) {
                            if (!releaseIface.hasOwnProperty(
                                iface.interfaceKey)) {
                              releaseIface[iface.interfaceKey] = 0;
                            }
                            releaseIface[iface.interfaceKey]++;
                          } else {
                            allOtherIfaces[iface.interfaceKey] = true;
                          }
                        },
                      }})
                      ));
                }
                return Promise.all(promises).then(() => {
                  // Find interfaces that all releases from "release" browser
                  // have shipped during the one-year grace period, but not
                  // shipped at any time in the grace period by any other
                  // browser.
                  let numBrowserSpecific = 0;
                  for (let ifaceKey in releaseIface) {
                    if (!releaseIface.hasOwnProperty(ifaceKey)) continue;
                    // Release browser shipping the API must have consistently
                    // shipped it in all "numPrevReleases" releases
                    if (releaseIface[ifaceKey] !== numPrevReleases) continue;
                    if (!allOtherIfaces.hasOwnProperty(ifaceKey)) {
                      numBrowserSpecific++;
                    }
                  }
                  return this.browserSpecificDAO.put(
                      this.BrowserSpecificData.create({
                        browserName: release.browserName,
                        release,
                        prevReleases,
                        comparedReleases,
                        numBrowserSpecific,
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
