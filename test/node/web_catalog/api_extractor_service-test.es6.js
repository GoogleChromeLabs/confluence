// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';


describe('ApiExtractorService', () => {
  let fs;
  let path;
  let objectGraphPath;
  let ApiExtractorService;
  let DAOContainer;

  beforeAll(() => {
    fs = require('fs');
    path = require('path');
    objectGraphPath = path.resolve(`${__dirname}/../../data`);
    ApiExtractorService = org.chromium.apis.web.ApiExtractorService;
    DAOContainer = org.chromium.apis.web.DAOContainer;
  });

  it('should use data from filename when "environment" data is missing', () => {
    const aes = ApiExtractorService.create(null, DAOContainer.create());
    const jsonFilePath = path.join(objectGraphPath,
                                   'window_Firefox_53.0_OSX_10.11.json');
    const json = JSON.parse(fs.readFileSync(jsonFilePath));
    const releaseInfo = aes.getReleaseInfo(jsonFilePath, json);
    expect(releaseInfo.browser).toBeDefined();
    expect(releaseInfo.platform).toBeDefined();
    expect(releaseInfo.browser.name).toBe('Firefox');
    expect(releaseInfo.browser.version).toBe('53.0');
    expect(releaseInfo.platform.name).toBe('OSX');
    expect(releaseInfo.platform.version).toBe('10.11');
  });

  it('should use data from "environment" when it is available', () => {
    const aes = ApiExtractorService.create(null, DAOContainer.create());
    const jsonFilePath = path.join(objectGraphPath,
                                   'window_IgnoreThisNameEntirely.json');
    const json = JSON.parse(fs.readFileSync(jsonFilePath));
    const releaseInfo = aes.getReleaseInfo(jsonFilePath, json);

    // Test file contains Chrome63/Linux64 data with browser and platform names
    // overridden as "Test".
    expect(releaseInfo.browser).toBeDefined();
    expect(releaseInfo.platform).toBeDefined();
    expect(releaseInfo.browser.name).toBe('Test');
    expect(releaseInfo.browser.version).toBe('63.0.3239.132');
    expect(releaseInfo.platform.name).toBe('Test');
    expect(releaseInfo.platform.version).toBe('x86.64');
  });

  it('should throw when "environment" data is missing and filename is malformed', () => {
    const aes = ApiExtractorService.create(null, DAOContainer.create());
    const jsonFilePath = path.join(objectGraphPath,
                                   'window_Name_is_Malformed.json');
    const json = JSON.parse(fs.readFileSync(jsonFilePath));
    expect(() => aes.getReleaseInfo(jsonFilePath, json)).toThrow();
  });
});
