// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('./aggressive_removal_data.es6.js');
require('./set_ops.es6.js');

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'AggressiveRemoval',
  extends: 'org.chromium.apis.web.MetricComputer',
  documentation: `AggressiveRemoval is a class that computes the aggressive
    removal metrics from browserApiDAO. It shows the number of APIs
    that is removed in this browser at least 1 years ago but still exists in
    other browsers.`,
  requires: [
    'foam.dao.EasyDAO',
    'org.chromium.apis.web.AggressiveRemovalData',
    'org.chromium.apis.web.Browser',
  ],
  properties: [
    {
      name: 'aggressiveRemovalDAO',
      documentation: `This is a DAO that contains AggressiveRemovalData.`,
      final: true,
      factory: function() {
        return this.EasyDAO.create({
          name: 'aggressiveRemovalDAO',
          of: this.AggressiveRemovalData,
          daoType: 'MDAO',
        });
      },
    },
  ],
  methods: [
    {
      name: 'compute',
      documentation: `Compute aggressive removal value for each browser in
          browsers at the given date.`,
      code: function(browsers, date) {
        return Promise.all(browsers.map(
            browser => this.computeForBrowser(browsers, date, browser)));
      },
    },
    {
      name: 'computeForBrowser',
      documentation: 'Compute aggressive removal value for a single browser.',
      code: function(browsers, date, browser) {
        let browserKeys = browsers.map((browser) => browser.browserKey);
        return Promise.all([
          // Current browsers from vendor other than "browser".
          this.browserDAO.where(this.mlang.AND(
              this.mlang.NEQ(this.Browser.BROWSER_NAME, browser.browserName),
              this.mlang.IN(this.Browser.BROWSER_KEY, browserKeys)))
                  .orderBy(this.mlang.DESC(this.Browser.RELEASE_DATE))
                  .select(),
          // Browsers released before the grace period "browser" vendor.
          this.browserDAO.where(this.mlang.AND(
                  this.mlang.EQ(this.Browser.BROWSER_NAME, browser.browserName),
                  this.mlang.LT(this.Browser.RELEASE_DATE,
                                new Date(date.getFullYear() - 1,
                                         date.getMonth(),
                                         date.getDate())),
                  this.mlang.EQ(this.Browser.OS_NAME, browser.osName)))
                      .orderBy(this.mlang.DESC(this.Browser.RELEASE_DATE))
                      .select(),
        ]).then(currentOtherAndPrevOwn => {
          let currentOtherBrowsers = currentOtherAndPrevOwn[0].a;
          let prevOwnBrowsers = currentOtherAndPrevOwn[1].a;
          return this.computeFromBrowsers(
              date, browser, currentOtherBrowsers, prevOwnBrowsers);
        });
      },
    },
    {
      name: 'computeFromBrowsers',
      documentation: `Compute aggressive removal value for "browser", given
          arrays of "current" browsers from other vendors and "previous"
          browsers (from "browser" vendor) that are older than the grace
          period.

          Let browserAPIs = <"browser" interfaces>
          Let curAPIs = INTERSECT(<"current" interfaces>)
          Let cutOffAPIs = <interfaces from newest releas in "previous">
          Let oldAPIs = UNION(<"previous" releases other than "cutOff" release>)

          Then:
          Removed up to grace period (and not added back) = removedAPIs =
              oldAPIs \ cutOffAPIs \ browserAPIs
          Aggressively removed APIs =
              INTERSECT(curAPIs, removedAPIs)`,
      code: function(date, browser, current, previous) {
        // If there are no other current browsers, or fewer than two previous
        // browsers, then the metric must be 0.
        if (current.length === 1 || previous.length < 2)
          return this.recordZero_(date, browser, current, previous);

        let prevLessOne = Array.from(previous);
        let postRemoval = prevLessOne.shift();
        let currentLessOne = Array.from(current);
        let firstCurrent = currentLessOne.shift();

        // Select from first current browser.
        return firstCurrent.interfaces.select(
            // Intersect with other current browsers to get APIs shipped by
            // all (non-"browser" vendor) current browsers.
            this.mlang.INTERSECT(
                currentLessOne.map(brwsr => brwsr.interfaces),
                // Intersect with removed-before-grace-period, not-added-back:
                // allAPIsOlderThanPostRemoval \ postRemoval \ browser.
                this.mlang.INTERSECT(
                    this.mlang.UNION.apply(
                        this.mlang,
                        prevLessOne.map(brwsr => brwsr.interfaces)),
                    this.mlang.SET_MINUS(
                        postRemoval.interfaces,
                        this.mlang.SET_MINUS(
                            browser.interfaces,
                            // Count APIs in set.
                            this.mlang.COUNT())))))
                                // Store count.
                                .then(count => {
                                  date; current; previous; firstCurrent;
                                  debugger;
                                  return this.aggressiveRemovalDAO.put(
                                      this.AggressiveRemovalData.create({
                                        browserName: browser.browserName,
                                        browserOneYearAgo: postRemoval,
                                        prevReleaseBrowsers: prevLessOne,
                                        currBrowsers: current,
                                        numAggressiveRemoval: count.value,
                                        date,
                                      }));
                                });
      },
    },
    {
      name: 'recordZero_',
      documentation: `Store 0-count in "aggressiveRemovalDAO" for given browser
          data.`,
      code: function(date, browser, current, previous) {
        // TODO(markdittmer): Currently the storage + UI layers expect
        // missing data when insufficient browser versions exist. Should we
        // instead store a 0-count?
      },
    },
  ],
});
