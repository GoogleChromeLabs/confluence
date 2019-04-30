// Copyright 2018 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('../object.es6.js');
require('../property.es6.js');
require('../action.es6.js');
require('./release.es6.js');

foam.CLASS({
  name: 'CompatProperty',
  package: 'org.chromium.apis.web',
  extends: 'foam.core.Boolean',

  documentation: `A Boolean property that contains a browser release as
      metadata. This pattern is used for per-API compat data.`,

  requires: ['org.chromium.apis.web.Release'],

  css: `
    .org-chromium-apis-web-CompatProperty {
      display: flex;
      justify-content: center;
      align-items: center;
    }

    .org-chromium-apis-web-CompatProperty.false {
      background-color: #FF8A80;
    }

    .org-chromium-apis-web-CompatProperty.true {
      background-color: #B9F6CA;
    }
  `,

  properties: [
    {
      class: 'FObjectProperty',
      of: 'org.chromium.apis.web.Release',
      name: 'release',
      documentation: `The browser release associated with this property.`,
    },
    {
      name: 'rawTableCellFormatter',
      documentation: `A raw markup outputter for table cells outputting this
          property's value.`,
      value: function(value, obj, axiom) {
        // Apply CSS class associated with this FOAM class.
        const cls = 'org-chromium-apis-web-CompatProperty';
        // Apply CSS class associated with value.
        const valueCls = value === true ? 'true' : value === false ?
              'false' : '';
        // Select Material icon text for checkmark, X, or hourglass (loading),
        // according to value.
        //
        // See https://material.io/icons/
        const iconValue = value === true ? 'check' : value === false ?
              'clear' : 'hourglass_empty';

        return `
          <div class="${cls} ${valueCls}">
            <span class="material-icons">${iconValue}</span>
          </div>
        `;
      },
    },
  ],

  methods: [
    {
      name: 'toString',
      documentation: `Provide something more useful than
          [FOAM class package path] as string representation`,
      code: function() {
        return this.name;
      },
    },
  ],
});

foam.CLASS({
  name: 'AbstractApiCompatData',
  package: 'org.chromium.apis.web',

  requires: ['org.chromium.apis.web.CompatProperty'],

  documentation: `Abstract class underpinning generated API compat data class

      This class is the basis for a generated class with a CompatProperty per
      release, detailing all the browser compatability data for a single
      API. This representation of the compat data is well aligned with FOAM
      features such as U2 table views, query parsers, MDAO indices, etc..`,

  properties: [
    {
      class: 'String',
      name: 'id',
      documentation: `ID of each datum is composed from interface + interface
          member name. Markup outputter enables CSS overlay pattern to
          conditionally overlay ID text over other table cells.`,
      label: 'API',
      expression: function(interfaceName, apiName) {
        return `${interfaceName}#${apiName}`;
      },
      hidden: true,
      rawTableCellFormatter: function(value, obj, axiom) {
        const textValue = foam.String.isInstance(value) ? value : '&nbsp;';
        return `
          <div class="id">
            <div class="overlay">${textValue}</div>
            <div class="ellipsis"><span>${textValue}</span></div>
          </div>
        `;
      },
    },
    {
      class: 'String',
      name: 'interfaceName',
      required: true,
    },
    {
      class: 'String',
      name: 'apiName',
      required: true,
    },
  ],
});

foam.CLASS({
  name: 'AbstractCompatClassGenerator',
  package: 'org.chromium.apis.web',

  documentation: `Component that encapsulates logic for generating a compat
      data class based on a collection of known releases.`,

  axioms: [foam.pattern.Singleton.create()],

  methods: [
    {
      name: 'generateSpec',
      documentation: `Generate the "spec" that would be passed to foam.CLASS()
          to declare a class of given [pkg].[name], based on given releases.`,
      code: function(pkg, name, releases) {
        const spec = {
          class: 'Model',
          package: pkg,
          name: name,
          extends: 'org.chromium.apis.web.AbstractApiCompatData',

          requires: ['org.chromium.apis.web.CompatProperty'],

          properties: [],
        };

        const propSpecs = releases.map((release) => {
          return {
            class: 'org.chromium.apis.web.CompatProperty',
            name: this.propertyNameFromRelease(release),
            label: this.propertyLabelFromRelease(release),
            release,
          };
        });
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
    {
      name: 'propertyNameFromRelease',
      documentation: `Produce a consistent property name from a release object.
          The property is used to store compat data for the release.`,
      code: function(release) {
        return (release.browserName.charAt(0).toLowerCase() +
                release.browserName.substr(1) +
                release.browserVersion +
                release.osName.charAt(0).toLowerCase() +
                release.osName.substr(1) +
                release.osVersion).replace(/[^a-z0-9]/ig, '_');
      },
    },
    {
      name: 'propertyLabelFromRelease',
      documentation: `Produce a label for use in UIs displaying compat data for
          the given release.`,
      code: function(release) {
        return release.browserName + ' ' +
          release.browserVersion + ' ' +
          release.osName + ' ' +
          release.osVersion;
      },
    },
  ],
});

foam.CLASS({
  name: 'CompatClassGenerator',
  package: 'org.chromium.apis.web',
  extends: 'org.chromium.apis.web.AbstractCompatClassGenerator',

  documentation: 'Singleton AbstractCompatClassGenerator',

  axioms: [foam.pattern.Singleton.create()],
});
