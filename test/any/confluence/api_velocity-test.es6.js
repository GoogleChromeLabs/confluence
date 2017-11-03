// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

describe('ApiVelocity', () => {
  function equals(a, b) {
    return foam.util.equals(a, b);
  }
  function sortedEquals(a, b) {
    function sort(array) { return array.sort(foam.util.compare); }
    return equals(sort(a), sort(b));
  }
  let EasyDAO;
  let DAOContainer;
  let Release;
  let ApiVelocity;
  let ApiVelocityData;
  let Junction;
  let WebInterface;
  let container;
  let runner;
  let releases;
  let ifaces;
  let junctions;
  beforeEach(() => {
    Release = foam.lookup('org.chromium.apis.web.Release');
    WebInterface = foam.lookup('org.chromium.apis.web.WebInterface');
    Junction = foam.lookup('org.chromium.apis.web.ReleaseWebInterfaceJunction');
    ApiVelocity = foam.lookup('org.chromium.apis.web.ApiVelocity');
    ApiVelocityData = foam.lookup('org.chromium.apis.web.ApiVelocityData');
    Junction = foam.lookup('org.chromium.apis.web.ReleaseWebInterfaceJunction');

    container = global.createDAOContainer();
    runner = global.createLocalRunner({
      metricComputerTypes: [
        foam.lookup('org.chromium.apis.web.MetricComputerType').API_VELOCITY,
      ],
    }, container);
    releases = container.releaseDAO;
    ifaces = container.webInterfaceDAO;
    junctions = container.releaseWebInterfaceJunctionDAO;
  });

  it('should compute single release', done => {
    let release = Release.create({
      browserName: 'Alpha',
      browserVersion: '1',
      osName: 'Windows',
      osVersion: '10',
      releaseDate: '2015-01-01T00:00:00.000Z'
    }, container);
    let iface = WebInterface.create({
      interfaceName: 'Alpha',
      apiName: 'iface',
    }, container);
    Promise.all([
      releases.put(release),
      ifaces.put(iface),
      junctions.put(Junction.create({
        id: [release.id, iface.id],
        sourceId: release.id,
        targetId: iface.id,
      })),
    ]).then(() => runner.run())
        .then(() => container.apiVelocityDAO.select())
        .then(sink => {
          expect(sortedEquals(sink.array, [
            ApiVelocityData.create({
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
    let iface1 = WebInterface.create({
      interfaceName: 'Alpha',
      apiName: 'iface1',
    }, container);
    let iface2 = WebInterface.create({
      interfaceName: 'Alpha',
      apiName: 'iface2',
    }, container);
    let iface3 = WebInterface.create({
      interfaceName: 'AlphaToOmega',
      apiName: 'iface',
    }, container);
    Promise.all([
      releases.put(release1),
      releases.put(release2),
      ifaces.put(iface1),
      ifaces.put(iface2),
      ifaces.put(iface3),
      // iface1 in release1 only.
      junctions.put(Junction.create({
        id: [release1.id, iface1.id],
        sourceId: release1.id,
        targetId: iface1.id,
      })),
      // iface2 in release1 and release2.
      junctions.put(Junction.create({
        id: [release1.id, iface2.id],
        sourceId: release1.id,
        targetId: iface2.id,
      })),
      junctions.put(Junction.create({
        id: [release2.id, iface2.id],
        sourceId: release2.id,
        targetId: iface2.id,
      })),
      // iface3 in release2 only.
      junctions.put(Junction.create({
        id: [release2.id, iface3.id],
        sourceId: release2.id,
        targetId: iface3.id,
      })),
    ]).then(() => runner.run())
        .then(() => container.apiVelocityDAO.select())
        .then(sink => {
          expect(sortedEquals(sink.array, [
            ApiVelocityData.create({
              releaseDate: release1.releaseDate,
              browserName: release1.browserName,
              currRelease: release1,
              totalApis: 2,
            }),
            ApiVelocityData.create({
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
    let iface1 = WebInterface.create({
      interfaceName: 'Alpha',
      apiName: 'iface',
    }, container);
    let iface2 = WebInterface.create({
      interfaceName: 'AlphaBeta',
      apiName: 'iface',
    }, container);
    let iface3 = WebInterface.create({
      interfaceName: 'Beta',
      apiName: 'iface',
    }, container);
    Promise.all([
      releases.put(alpha),
      releases.put(beta),
      ifaces.put(iface1),
      ifaces.put(iface2),
      ifaces.put(iface3),
      // iface1 in alpha only.
      junctions.put(Junction.create({
        id: [alpha.id, iface1.id],
        sourceId: alpha.id,
        targetId: iface1.id,
      })),
      // iface2 in alpha and beta.
      junctions.put(Junction.create({
        id: [alpha.id, iface2.id],
        sourceId: alpha.id,
        targetId: iface2.id,
      })),
      junctions.put(Junction.create({
        id: [beta.id, iface2.id],
        sourceId: beta.id,
        targetId: iface2.id,
      })),
      // iface3 in beta only.
      junctions.put(Junction.create({
        id: [beta.id, iface3.id],
        sourceId: beta.id,
        targetId: iface3.id,
      })),
    ]).then(() => runner.run())
        .then(() => container.apiVelocityDAO.select())
        .then(sink => {
          expect(sortedEquals(sink.array, [
            ApiVelocityData.create({
              releaseDate: alpha.releaseDate,
              browserName: alpha.browserName,
              currRelease: alpha,
              totalApis: 2,
            }),
            ApiVelocityData.create({
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
