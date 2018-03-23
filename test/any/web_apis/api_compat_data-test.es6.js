// Copyright 2018 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

describe('CompatProperty', () => {
  let pkg;
  beforeEach(() => {
    pkg = org.chromium.apis.web;
    foam.CLASS({
      name: 'CompatPropertyContainer',
      package: 'org.chromium.apis.web.test',

      properties: [
        {
          class: 'org.chromium.apis.web.CompatProperty',
          name: 'prop',
        },
      ],
    });
  });

  describeRawTableCellFormatterTests(
      'org.chromium.apis.web.test.CompatPropertyContainer', 'prop',
      () => org.chromium.apis.web.test.CompatPropertyContainer.create());
});

describe('AbstractApiCompatData', () => {
  let pkg;
  beforeEach(() => {
    pkg = org.chromium.apis.web;
  });

  it('should require interfaceName and apiName', () => {
    expect(() => pkg.AbstractApiCompatData.create({apiName: 'a'}).validate())
        .toThrow();
    expect(() => pkg.AbstractApiCompatData.create({interfaceName: 'i'})
           .validate()).toThrow();
    expect(() => pkg.AbstractApiCompatData.create({
      interfaceName: 'i',
      apiName: 'a',
    })).not.toThrow();
  });

  it('should format ID as string: [interfaceName]#[apiName]', () => {
    expect(pkg.AbstractApiCompatData.create({
      interfaceName: 'i',
      apiName: 'a',
    }).id).toBe('i#a');
  });

  describeRawTableCellFormatterTests(
      'org.chromium.apis.web.AbstractApiCompatData', 'id',
      () => org.chromium.apis.web.AbstractApiCompatData.create({
        interfaceName: 'i',
        apiName: 'a',
      }));
});
