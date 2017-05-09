// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

describe('VersionHistory', function() {
  let versionHistory;
  beforeEach(function() {
    versionHistory = org.chromium.apis.web.VersionHistory.create({
      releaseHistory: {
        'Chrome': {
          '54': '2016-10-12',
          '55': '2016-12-01',
          '56': '2017-01-25',
        },
        'Safari': {
          '601.7.8': '2016-09-01',
          '602.1.50': '2016-09-20',
          '602.4.8': '2017-01-23',
        },
      },
    });
  });

  it('returns release date for a given release.', function() {
    expect(versionHistory.getReleaseDate('Chrome', '56.0.2924.87'))
      .toEqual(new Date('2017-01-25'));
    expect(versionHistory.getReleaseDate('Chrome', '55.0.2883.75'))
      .toEqual(new Date('2016-12-01'));
    expect(versionHistory.getReleaseDate('Safari', '602.4.8'))
      .toEqual(new Date('2017-01-23'));
  });

  it('throws error if the given version is not in version history.',
    function() {
      expect(() => versionHistory.getReleaseDate('Chrome', '10.0'))
        .toThrow(new Error('Version 10.0 not found in Chrome history.'));
    });

  it('throws error if the given release is not in version history.',
    function() {
      expect(() => versionHistory.getReleaseDate('UC', '10.0'))
        .toThrow(new Error('UC not found in release history.'));
    });
});
