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

fdescribe('CompatClassGenerator', () => {
  let CompatProperty;
  let Release;
  let generator;
  beforeEach(() => {
    CompatProperty = org.chromium.apis.web.CompatProperty;
    Release = org.chromium.apis.web.Release;
    generator = org.chromium.apis.web.CompatClassGenerator.create();
  });

  it('should register a class', () => {
    generator.generateClass(
        generator.generateSpec('org.chromium.apis.web', 'CompatClass', []));
    expect(foam.lookup('org.chromium.apis.web.CompatClass')).toBeDefined();
  });

  it('should accept a model', () => {
    const model = foam.json.parse(
      generator.generateSpec('org.chromium.apis.web', 'CompatClass', []));
    expect(foam.core.Model.isInstance(model)).toBe(true);
    generator.generateClass(model);
    expect(foam.lookup('org.chromium.apis.web.CompatClass')).toBeDefined();
  });

  it('should create a property for each release',  () => {
    const release0 = Release.create({
      browserName: 'Alpha',
      browserVersion:'1.2.3',
      osName: 'Zulu',
      osVersion: '9.8.7',
    });
    const release1 = Release.create({
      browserName: 'Beta',
      browserVersion:'3.2.1',
      osName: 'Yankee',
      osVersion: '7.8.9',
    });
    const Cls = generator.generateClass(
        generator.generateSpec('org.chromium.apis.web', 'CompatClass', [
          release0,
          release1,
        ]));
    const props = Cls.getAxiomsByClass(CompatProperty);
    expect(props.length).toBe(2);
    expect(foam.util.equals(props[0].release, release0)).toBe(true);
    expect(foam.util.equals(props[1].release, release1)).toBe(true);
  });

  it('should error out validating with duplicate relases', () => {
    expect(() => generator.generateClass(
        generator.generateSpec('org.chromium.apis.web', 'CompatClass', [
          Release.create({
            browserName: 'Alpha',
            browserVersion:'1.2.3',
            osName: 'Zulu',
            osVersion: '9.8.7',
          }),
          Release.create({
            browserName: 'Alpha',
            browserVersion:'1.2.3',
            osName: 'Zulu',
            osVersion: '9.8.7',
          }),
        ]))).toThrow();
  });

  it('should disambiguate similar versions', () => {
    const release0 = Release.create({
      browserName: 'Alpha',
      browserVersion:'1.2.3',
      osName: 'Zulu',
      osVersion: '9.8.7',
    });
    const release1 = Release.create({
      browserName: 'Alpha',
      browserVersion:'1.23',
      osName: 'Zulu',
      osVersion: '9.8.7',
    });
    const Cls = generator.generateClass(
        generator.generateSpec('org.chromium.apis.web', 'CompatClass', [
          release0,
          release1,
        ]));
    const props = Cls.getAxiomsByClass(CompatProperty);
    expect(props.length).toBe(2);
    expect(foam.util.equals(props[0].release, release0)).toBe(true);
    expect(foam.util.equals(props[1].release, release1)).toBe(true);
  });
});
