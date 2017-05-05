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
  let ReleaseApiContainer;
  let Release;
  let ApiVelocity;
  let ApiVelocityData;
  let Junction;
  let WebInterface;
  let container;
  let releases;
  let apiVelocity;
  let ifaces;
  let junctions;
  beforeEach(() => {
    Release = foam.lookup('org.chromium.apis.web.Release');
    WebInterface = foam.lookup('org.chromium.apis.web.WebInterface');
    Junction = foam.lookup('org.chromium.apis.web.ReleaseWebInterfaceJunction');
    ApiVelocity = foam.lookup('org.chromium.apis.web.ApiVelocity');
    ApiVelocityData = foam.lookup('org.chromium.apis.web.ApiVelocityData');
    Junction = foam.lookup('org.chromium.apis.web.ReleaseWebInterfaceJunction');

    container = global.createReleaseApiContainer();
    releases = container.releaseDAO;
    apiVelocity = ApiVelocity.create(null, container);
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
    ]).then(() => apiVelocity.run())
        .then(() => apiVelocity.apiVelocityDAO.select())
        .then(sink => {
          expect(sortedEquals(sink.a, [
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
});
