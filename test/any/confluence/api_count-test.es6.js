// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

describe('ApiCount', () => {
  function equals(a, b) {
    return foam.util.equals(a, b);
  }
  function sortedEquals(a, b) {
    function sort(array) { return array.sort(foam.util.compare); }
    return equals(sort(a), sort(b));
  }
  let Release;
  let ApiCountData;
  let CompatData;
  let container;
  let gen;
  let runner;
  let releases;
  let compatData;

  beforeEach(() => {
    gen =
        foam.lookup('org.chromium.apis.web.AbstractCompatClassGenerator')
        .create()
  });

  const init = releaseSpecs => {
    // Register custom CompatData before looking up classes and instantiating
    // instances.
    foam.CLASS({
      name: 'CompatData',
      package: 'org.chromium.apis.web.test',
      extends: 'org.chromium.apis.web.AbstractApiCompatData',

      properties: releaseSpecs.map(r => {
        return {
          class: 'org.chromium.apis.web.CompatProperty',
          release: r,
          name: gen.propertyNameFromRelease(r),
          label: gen.propertyLabelFromRelease(r),
        };
      }),
    });
    foam.register(org.chromium.apis.web.test.CompatData, 'org.chromium.apis.web.generated.CompatData');
    CompatData = foam.lookup('org.chromium.apis.web.generated.CompatData');

    Release = foam.lookup('org.chromium.apis.web.Release');
    ApiCountData = foam.lookup('org.chromium.apis.web.ApiCountData');

    container = global.createDAOContainer();
    runner = global.createLocalRunner({
      metricComputerTypes: [
        foam.lookup('org.chromium.apis.web.MetricComputerType').API_COUNT,
      ],
    }, container);
    releases = container.releaseDAO;
    compatData = container.compatDAO;

    return releaseSpecs.map(rs => Release.create(rs, container));
  };

  it('should compute single release', done => {
    let release = {
      browserName: 'Alpha',
      browserVersion: '1',
      osName: 'Windows',
      osVersion: '10',
      releaseDate: '2015-01-01T00:00:00.000Z'
    };
    [release] = init([release]);

    let compat = CompatData.create({
      interfaceName: 'Alpha',
      apiName: 'iface',
      [gen.propertyNameFromRelease(release)]: true,
    })
    Promise.all([
      releases.put(release),
      compatData.put(compat),
    ]).then(() => runner.run())
        .then(() => container.apiCountDAO.select())
        .then(sink => {
          expect(sortedEquals(sink.array, [
            ApiCountData.create({
              releaseDate: release.releaseDate,
              browserName: release.browserName,
              currRelease: release,
              totalApis: 1,
            }),
          ])).toBe(true);
          done();
        });
  });

  it('should compute two releases from the same browser', done => {
    let release1 = Release.create({
      browserName: 'Alpha',
      browserVersion: '1',
      osName: 'Windows',
      osVersion: '10',
      releaseDate: '2015-01-01T00:00:00.000Z'
    }, container);
    let release2 = Release.create({
      browserName: 'Alpha',
      browserVersion: '2',
      osName: 'Windows',
      osVersion: '10',
      releaseDate: '2016-01-01T00:00:00.000Z'
    }, container);
    [release1, release2] = init([release1, release2]);
    let compat1 = CompatData.create({
      interfaceName: 'Alpha',
      apiName: 'iface1',
      [gen.propertyNameFromRelease(release1)]: true,
    }, container);
    let compat2 = CompatData.create({
      interfaceName: 'Alpha',
      apiName: 'iface2',
      [gen.propertyNameFromRelease(release1)]: true,
      [gen.propertyNameFromRelease(release2)]: true,
    }, container);
    let compat3 = CompatData.create({
      interfaceName: 'AlphaToOmega',
      apiName: 'iface',
      [gen.propertyNameFromRelease(release2)]: true,
    }, container);
    Promise.all([
      releases.put(release1),
      releases.put(release2),
      compatData.put(compat1),
      compatData.put(compat2),
      compatData.put(compat3),
    ]).then(() => runner.run())
        .then(() => container.apiCountDAO.select())
        .then(sink => {
          expect(sortedEquals(sink.array, [
            ApiCountData.create({
              releaseDate: release1.releaseDate,
              browserName: release1.browserName,
              currRelease: release1,
              totalApis: 2,
            }),
            ApiCountData.create({
              releaseDate: release2.releaseDate,
              browserName: release2.browserName,
              currRelease: release2,
              prevRelease: release1,
              totalApis: 2,
              removedApis: 1,
              newApis: 1,
            }),
          ])).toBe(true);
          done();
        });
  });

  it('should compute two releases from different browsers', done => {
    let alpha = Release.create({
      browserName: 'Alpha',
      browserVersion: '1',
      osName: 'Windows',
      osVersion: '10',
      releaseDate: '2015-01-01T00:00:00.000Z'
    }, container);
    let beta = Release.create({
      browserName: 'Beta',
      browserVersion: '1',
      osName: 'Windows',
      osVersion: '10',
      releaseDate: '2015-01-01T00:00:00.000Z'
    }, container);
    [alpha, beta] = init([alpha, beta]);

    let compat1 = CompatData.create({
      interfaceName: 'Alpha',
      apiName: 'iface',
      [gen.propertyNameFromRelease(alpha)]: true,
    }, container);
    let compat2 = CompatData.create({
      interfaceName: 'AlphaBeta',
      apiName: 'iface',
      [gen.propertyNameFromRelease(alpha)]: true,
      [gen.propertyNameFromRelease(beta)]: true,
    }, container);
    let compat3 = CompatData.create({
      interfaceName: 'Beta',
      apiName: 'iface',
      [gen.propertyNameFromRelease(beta)]: true,
    }, container);
    Promise.all([
      releases.put(alpha),
      releases.put(beta),
      compatData.put(compat1),
      compatData.put(compat2),
      compatData.put(compat3),
    ]).then(() => runner.run())
        .then(() => container.apiCountDAO.select())
        .then(sink => {
          expect(sortedEquals(sink.array, [
            ApiCountData.create({
              releaseDate: alpha.releaseDate,
              browserName: alpha.browserName,
              currRelease: alpha,
              totalApis: 2,
            }),
            ApiCountData.create({
              releaseDate: beta.releaseDate,
              browserName: beta.browserName,
              currRelease: beta,
              totalApis: 2,
            }),
          ])).toBe(true);
          done();
        });
  });
});
