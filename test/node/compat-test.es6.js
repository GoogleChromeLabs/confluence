// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

describe('ClassGenerator', () => {
  it('should load compat data class from file', done => {
    org.chromium.apis.web.ClassGenerator.create({
      classURL: `file://${__dirname}/../data/class:org.chromium.apis.web.generated.CompatData.json`,
    }).generateClass().then(cls => {
      expect(cls.id).toBe('org.chromium.apis.web.generated.CompatData');
      done();
    }, done.fail);
  });
})
