// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('./api_velocity_data.es6');

foam.CLASS({
  name: 'ApiVelocity',
  package: 'org.chromium.apis.web',
  implements: ['org.chromium.mlang.Expressions'],

  documentation: `Metric computer for API counts that Nth release to N-1th
      release of a browser.

      Counts are: {<total APIs>, <removed APIs>, <added APIs>}.`,

  requires: [
    'foam.dao.ArraySink',
    'foam.dao.EasyDAO',
    'org.chromium.apis.web.Release',
  ],
  imports: ['releaseDAO'],

  properties: [
    {
      class: 'FObjectProperty',
      of: 'foam.dao.DAO',
      name: 'apiVelocityDAO',
      documentation: `This is a DAO that contains ApiVelocityData.`,
      final: true,
      factory: function() {
        return this.EasyDAO.create({
          name: 'apiVelocityDAO',
          of: this.ApiVelocityData,
          daoType: 'MDAO',
        });
      },
    },
  ],
  methods: [
    {
      name: 'init',
      documentation: `The init function computes the API Velocity for
          each major releases contained in the given releaseWebInterfaceJunctionDAO and
          store them in apiVelocityDAO.`,
      code: function() {
        this.releaseDAO.orderBy(this.Release.RELEASE_DATE)
            .select(this.GROUP_BY(
                this.Release.BROWSER_NAME,
                this.GROUP_BY(
                    this.Release.OS_NAME,
                    this.ArraySink.create())))
            .then((groups) => {
              for (let i = 0; i < groups.groupKeys.length; i++) {
                let browserName = groups.groupKeys[i];
                // Use Windows platform if this release is available in Windows.
                // Use whatever available if Windows is not supported.
                let releaseOS = 'Windows';
                if (!groups.groups[browserName].groups.hasOwnProperty(
                    releaseOS)) {
                  releaseOS = groups.groups[browserName].groupKeys[0];
                }
                let releases = groups.groups[browserName].groups[releaseOS].a;
                let prevIfaceSelect = null;
                for (let i = 0; i < releases.length; i++) {
                  let release = releases[i];
                  // Continue when no previous release is found.
                  if (i === 0) {
                    prevIfaceSelect = release.interfaces.select();
                    prevIfaceSelect.then((arraySink) => {
                      this.apiVelocityDAO.put(this.ApiVelocityData.create({
                        releaseDate: release.releaseDate,
                        browserName: release.browserName,
                        prevRelease: null,
                        currRelease: release,
                        totalApis: arraySink.a.length,
                        newApis: 0,
                        removedApis: 0,
                      }));
                    });
                    continue;
                  }
                  // Reuse selected data by passing promise to next iteration.
                  let tempPromise = prevIfaceSelect;
                  prevIfaceSelect = release.interfaces.select();
                  Promise.all([tempPromise, prevIfaceSelect]).then((result) => {
                    // prevIfaceDict initialized as all previous, but pared
                    // down when found in curIfaces, yielding only removed
                    // interfaces.
                    let prevIfaces = result[0].a;
                    let currIfaces = result[1].a;
                    let prevIfaceDict = {};
                    let newApis = 0;
                    let removedApis = 0;
                    for (let j = 0; j < prevIfaces.length; j++) {
                      prevIfaceDict[prevIfaces[j].interfaceKey] = true;
                    }
                    for (let j = 0; j < currIfaces.length; j++) {
                      let ifaceKey = currIfaces[j].interfaceKey;
                      if (prevIfaceDict[ifaceKey]) {
                        delete prevIfaceDict[ifaceKey];
                      } else {
                        // This interface exists in current version
                        // not in previous version.
                        newApis++;
                      }
                    }
                    removedApis = Object.keys(prevIfaceDict).length;
                    this.apiVelocityDAO.put(this.ApiVelocityData.create({
                      releaseDate: release.releaseDate,
                      browserName: release.browserName,
                      prevRelease: releases[i - 1],
                      currRelease: release,
                      totalApis: currIfaces.length,
                      newApis,
                      removedApis,
                    }));
                  });
                }
              }
            });
      },
    },
  ],
});
