// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';


describe('ObjectGraphImporter', () => {
  let path;
  let objectGraphPath;
  let ObjectGraphImporter;

  beforeAll(() => {
    path = require('path');
    objectGraphPath = path.resolve(`${__dirname}/../data`);
    ObjectGraphImporter = org.chromium.apis.web.ObjectGraphImporter;
  });

  it('should use data from filename when "environment" data is missing', () => {
    const ogi = ObjectGraphImporter.create({objectGraphPath});
    const releaseInfo = ogi.getReleaseInfo('window_Firefox_53.0_OSX_10.11.json');
    expect(releaseInfo.browser).toBeDefined();
    expect(releaseInfo.platform).toBeDefined();
    expect(releaseInfo.browser.name).toBe('Firefox');
    expect(releaseInfo.browser.version).toBe('53.0');
    expect(releaseInfo.platform.name).toBe('OSX');
    expect(releaseInfo.platform.version).toBe('10.11');
  });

  it('should use data from "environment" when it is available', () => {
    const ogi = ObjectGraphImporter.create({objectGraphPath});

    // Test file contains Chrome63/Linux64 data with browser and platform names
    // overridden as "Test".
    const releaseInfo = ogi.getReleaseInfo('window_IgnoreThisNameEntirely.json');
    expect(releaseInfo.browser).toBeDefined();
    expect(releaseInfo.platform).toBeDefined();
    expect(releaseInfo.browser.name).toBe('Test');
    expect(releaseInfo.browser.version).toBe('63.0.3239.132');
    expect(releaseInfo.platform.name).toBe('Test');
    expect(releaseInfo.platform.version).toBe('x86.64');
  });

  it('should throw when "environment" data is missing and filename is malformed', () => {
    const ogi = ObjectGraphImporter.create({objectGraphPath});
    expect(() => ogi.getReleaseInfo('window_Name_is_Malformed.json')).toThrow();
  });
});
