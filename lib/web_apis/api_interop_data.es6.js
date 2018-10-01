// Copyright 2018 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('../action.es6.js');
require('../object.es6.js');
require('../property.es6.js');
require('./api_compat_data.es6.js');
require('./release.es6.js');

foam.CLASS({
  name: 'InteropProperty',
  package: 'org.chromium.apis.web',
  extends: 'foam.core.Int',

  documentation: `An integer property that contains a the number of releases.`,

  requires: ['org.chromium.apis.web.Release'],

  properties: [
    {
      class: 'FObjectArray',
      of: 'org.chromium.apis.web.Release',
      name: 'releases',
      documentation: `The browser releases associated with this property.`,
    },
    {
      class: 'Date',
      name: 'releaseDate',
    },
  ],
});

foam.CLASS({
  name: 'InteropPropertyGenerator',
  package: 'org.chromium.apis.web',

  properties: [
    {
      class: 'FObjectArray',
      of: 'org.chromium.apis.web.Release',
      name: 'releases',
      required: true,
    },
    {
      name: 'releaseSeqNos_',
      expression: function(releases) {
        let dateMap = new Map();
        let relMap = new Map();
        for (const release of releases) {
          const dateKey = release.releaseDate.getTime();
          if (typeof dateMap.get(dateKey) !== 'number') {
            dateMap.set(dateKey, 0);
          }
          const num = dateMap.get(dateKey);
          relMap.set(release.id, num);
          dateMap.set(dateKey, num + 1);
        }
        return relMap;
      },
    },
  ],

  methods: [
    {
      name: 'propertyNameFromRelease',
      code: function (release) {
        return `r${release.releaseDate.getTime()}_${this.releaseSeqNos_.get(release.id)}`;
      },
    },
    {
      name: 'propertyLabelFromRelease',
      code: function(release) {
        return `${release.releaseDate.toDateString()} #${this.releaseSeqNos_.get(release.id)}`;
      },
    },
  ],
})

foam.CLASS({
  name: 'AbstractInteropClassGenerator',
  package: 'org.chromium.apis.web',

  requires: [
    'org.chromium.apis.web.InteropPropertyGenerator',
    'org.chromium.apis.web.generated.CompatData',
  ],

  methods: [
    {
      name: 'generateSpec',
      // NOTE: Releases must be in a deterministiccronological release date order.
      code: function(pkg, name, releases) {
        const gen = this.InteropPropertyGenerator.create({releases});
        let spec = {
          class: 'Model',
          package: pkg,
          name: name,
          extends: 'org.chromium.apis.web.AbstractApiCompatData',

          requires: ['org.chromium.apis.web.InteropProperty'],

          properties: [],
        };

        let propSpecs = [];
        let currentReleasesMap = new Map();
        let i;
        for (i = 0; currentReleasesMap.size < 4; i++) {
          currentReleasesMap.set(releases[i].browserName, releases[i]);
        }

        for (i--; i < releases.length; i++) {
          const release = releases[i];
          currentReleasesMap.set(release.browserName, release);
          const currentReleases = Array.from(currentReleasesMap.values());
          let cols = [];
          for (const release of currentReleases) {
            cols.push(this.CompatData.getAxiomByName(
                gen.propertyNameFromRelease(release)));
          }

          propSpecs.push({
            class: 'org.chromium.apis.web.InteropProperty',
            name: gen.propertyNameFromRelease(release),
            label: gen.propertyLabelFromRelease(release),
            releases: Array.from(currentReleases),
            releaseDate: release.releaseDate,
          });
        }

        spec.properties = spec.properties.concat(propSpecs);

        return spec;
      },
    },
    {
      name: 'generateClass',
      documentation: `Generate a class based on a spec or foam.core.Model. The
          former is used during data generation phase; the latter is used when
          loading a model for existing data.`,
      code: function(specOrModel) {
        if (foam.core.Model.isInstance(specOrModel)) {
          // Run post-model instantiation steps from:
          // https://github.com/foam-framework/foam2/blob/632c6467c0a7f123a2270cc549db5d8e3d93b574/src/foam/core/Boot.js#L201
          const model = specOrModel;
          model.validate();
          const cls = model.buildClass();
          cls.validate();
          foam.register(cls);
          foam.package.registerClass(cls);
          return cls;
        } else {
          // Declare class from spec as usual.
          const spec = specOrModel;
          foam.CLASS(spec);
          return foam.lookup(`${spec.package}.${spec.name}`);
        }
      },
    },
  ],
});

foam.CLASS({
  name: 'InteropClassGenerator',
  package: 'org.chromium.apis.web',
  extends: 'org.chromium.apis.web.AbstractInteropClassGenerator',

  documentation: 'Singleton AbstractInteropClassGenerator',

  axioms: [foam.pattern.Singleton.create()],
});
