// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

describe('RelationalToCompatConverter', () => {
  let pkg;
  let releaseAlpha;
  let releaseBeta;
  let compatDataSpec;
  let compatDataCls;
  let classGenerator;
  let converter;
  beforeEach(() => {
    pkg = org.chromium.apis.web;
    releaseAlpha = pkg.Release.create({
      browserName: 'Alpha',
      browserVersion: '1',
      osName: 'Window',
      osVersion: '10.0',
    });
    releaseBeta = pkg.Release.create({
      browserName: 'Beta',
      browserVersion: '1',
      osName: 'Window',
      osVersion: '10.0',
    });
    compatDataSpec = {
      name: 'CompatData',
      package: 'org.chromium.apis.web.test',
      extends: 'org.chromium.apis.web.AbstractApiCompatData',

      properties: [
        {
          class: 'org.chromium.apis.web.CompatProperty',
          name: 'alpha1windows10_0',
          label: 'Alpha 1 Windows 10.0',
          release: releaseAlpha,
        },
        {
          class: 'org.chromium.apis.web.CompatProperty',
          name: 'beta1windows10_0',
          label: 'Beta 1 Windows 10.0',
          release: releaseBeta,
        },
      ],
    };
    foam.CLASS(compatDataSpec);
    compatDataCls = foam.lookup('org.chromium.apis.web.test.CompatData');

    foam.CLASS({
      name: 'FakeCompatClassGenerator',
      package: 'org.chromium.apis.web.test',
      extends: 'org.chromium.apis.web.AbstractCompatClassGenerator',

      methods: [
        function generateSpec() {
          return compatDataSpec;
        },
        function generateClass() {
          return compatDataCls;
        },
      ],
    });

    classGenerator =
        foam.lookup('org.chromium.apis.web.test.FakeCompatClassGenerator')
            .create();
    converter = pkg.AbstractRelationalToCompatConverter.create({
      classGenerator,
    });
  });

  it('should set bit from join', () => {
    // +-------------------------+
    // | API:    | Alpha | Beta  |
    // +-------------------------+
    // | I.alpha | true  | false |
    // | I.beta  | false | true  |
    // +-------------------------+
    const apiAlpha = pkg.WebInterface.create({
      interfaceName: 'I',
      apiName: 'alpha',
    });
    const apiBeta = pkg.WebInterface.create({
      interfaceName: 'I',
      apiName: 'beta',
    });
    const junctionAlpha = pkg.ReleaseWebInterfaceJunction.create({
      sourceId: releaseAlpha.id,
      targetId: apiAlpha.id,
    });
    const junctionBeta = pkg.ReleaseWebInterfaceJunction.create({
      sourceId: releaseBeta.id,
      targetId: apiBeta.id,
    });

    const clsAndData = converter.convert(
        [releaseAlpha, releaseBeta],
        [apiAlpha, apiBeta],
        [junctionAlpha, junctionBeta]);

    expect(clsAndData.cls).toBe(compatDataCls);
    expect(clsAndData.data.length).toBe(2);
    expect(clsAndData.data[0].alpha1windows10_0).toBe(true);
    expect(clsAndData.data[0].beta1windows10_0).toBe(false);
    expect(clsAndData.data[1].alpha1windows10_0).toBe(false);
    expect(clsAndData.data[1].beta1windows10_0).toBe(true);
  });

  it('should create data row for unshipped API', () => {
    // +-------------------------+
    // | API:    | Alpha | Beta  |
    // +-------------------------+
    // | I.x     | true  | false |
    // | I.y     | false | false |
    // +-------------------------+
    const apiAlpha = pkg.WebInterface.create({
      interfaceName: 'I',
      apiName: 'x',
    });
    const apiBeta = pkg.WebInterface.create({
      interfaceName: 'I',
      apiName: 'y',
    });
    const junctionAlpha = pkg.ReleaseWebInterfaceJunction.create({
      sourceId: releaseAlpha.id,
      targetId: apiAlpha.id,
    });

    const clsAndData = converter.convert(
        [releaseAlpha, releaseBeta],
        [apiAlpha, apiBeta],
        [junctionAlpha]);

    expect(clsAndData.cls).toBe(compatDataCls);
    expect(clsAndData.data.length).toBe(2);
    expect(clsAndData.data[0].alpha1windows10_0).toBe(true);
    expect(clsAndData.data[0].beta1windows10_0).toBe(false);
    expect(clsAndData.data[1].alpha1windows10_0).toBe(false);
    expect(clsAndData.data[1].beta1windows10_0).toBe(false);
  });

  it('should throw on join for unknown API', () => {
    const apiAlpha = pkg.WebInterface.create({
      interfaceName: 'I',
      apiName: 'alpha',
    });
    const apiBeta = pkg.WebInterface.create({
      interfaceName: 'I',
      apiName: 'beta',
    });
    const apiUnknown = pkg.WebInterface.create({
      interfaceName: 'Un',
      apiName: 'known',
    });
    const junctionUnknown = pkg.ReleaseWebInterfaceJunction.create({
      sourceId: releaseAlpha.id,
      targetId: apiUnknown.id,
    });

    expect(() => converter.convert(
        [releaseAlpha, releaseBeta],
        [apiAlpha, apiBeta],
        [junctionUnknown])).toThrow();
  });
});
