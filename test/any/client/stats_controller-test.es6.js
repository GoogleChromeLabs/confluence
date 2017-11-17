// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

describe('StatsController', () => {
  beforeEach(() => {
    foam.CLASS({
      package: 'org.chromium.apis.web.test',
      name: 'StatsController',
      extends:   'org.chromium.apis.web.StatsController',

      properties: [
        {
          class: 'Boolean',
          name: 'reported',
        },
        {
          class: 'Function',
          name: 'reportStat',
          factory: function() {
            return function(key, value) {
              expect(this.reported).toBe(false);
              this.reported = true;
            };
          },
        },
      ],

      listeners: [
        {
          name: 'onDisplayMatrix_',
          isMerged: true,
          mergeDelay: 10,
          code: function(sub, topic, matrix, time) {
            this.SUPER(sub, topic, matrix, time);
          },
        },
      ],
    });
  });

  it('should record display matrix time exactly once on catalog page', done => {
    const ctrl = org.chromium.apis.web.test.StatsController.create({
      initialHash: '#!/catalog',
    });
    ctrl.onDisplayMatrix({});
    ctrl.onDisplayMatrix({});
    ctrl.onDisplayMatrix({});
    setTimeout(() => {
      expect(ctrl.reported).toBe(true);
      done();
    }, 30);
  });

  it('should not record display matrix on non-catalog page', done => {
    const ctrl = org.chromium.apis.web.test.StatsController.create({
      initialHash: '#!/confluence',
    });
    ctrl.onDisplayMatrix({});
    ctrl.onDisplayMatrix({});
    ctrl.onDisplayMatrix({});
    setTimeout(() => {
      expect(ctrl.reported).toBe(false);
      done();
    }, 30);
  });
});
