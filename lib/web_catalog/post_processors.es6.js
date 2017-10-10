// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

foam.INTERFACE({
  name: 'ApiCatalogPostProcessor',
  package: 'org.chromium.apis.web',

  documentation: `JSON<apiCatalog> mutators. Implementations perform
      post-processing steps on an "apiCatalog" extracted from ObjectGraph "og".
      When such post-processors are in place to deal with web platform
      implementation problems, issue/bug link metadata are included to ease
      tracking changes/fixes that may obsolete the post-processor.`,

  properties: [
    {
      class: 'String',
      name: 'confluenceIssueURL',
      documentation: `URL for issue tracking post-processor in API Confluence
          repository, if any.`,
      value: null,
    },
    {
      class: 'String',
      name: 'browserBugURL',
      documentation: `URL for bug tracking spec conformance problem that led to
          the need for this post-processor, if any.`,
      value: null,
    },
    {
      class: 'String',
      name: 'specURL',
      documentation: 'URL for spec related to post-processor, if any.',
      value: null,
    },
    {
      class: 'Array',
      of: 'String',
      name: 'otherURLs',
      documentation: 'Additional URLs related to post-processor, if any.',
    },
  ],

  methods: [
    {
      name: 'postProcess',
      documentation: `Remove blacklist properties and add
        Function and Object's APIs.`,
      args: [
        {
          documentation: `The json stores interface and API data.`,
          name: 'apiCatalog',
          typeName: 'JSON',
        },
        {
          documentation: `The object graph.`,
          name: 'og',
          typeName: 'ObjectGraph',
        },
      ],
      code: function(apiCatalog, og) {
        throw new Error('Missing ApiCatalogPostProcessor.postProcess() implementation');
      },
    },
  ],
});

foam.CLASS({
  name: 'RemoveInterfacesProcessor',
  package: 'org.chromium.apis.web',
  implements: ['org.chromium.apis.web.ApiCatalogPostProcessor'],

  properties: [
    {
      class: 'StringArray',
      name: 'interfaceNames',
      documentation: `Blacklisted properties that will not be visited or
        stored in final API catalog.`,
      value: null,
      required: true,
    },
  ],

  methods: [
    function postProcess(apiCatalog, og) {
      this.validate();

      for (let i = 0; i < this.interfaceNames.length; i++) {
        let iface = this.interfaceNames[i];
        if (apiCatalog[iface]) {
          delete apiCatalog[iface];
        }
      }
    }
  ]
});

foam.CLASS({
  name: 'RemoveApisProcessor',
  package: 'org.chromium.apis.web',
  extends: 'org.chromium.apis.web.RemoveInterfacesProcessor',
  implements: ['org.chromium.apis.web.ApiCatalogPostProcessor'],

  properties: [
    {
      name: 'apiNameRegExp',
      documentation: `Regular expression matching API names that should be
          removed.`,
      value: /^$/,
      required: true,
    },
  ],

  methods: [
    function postProcess(apiCatalog, og) {
      this.validate();

      const apiNameRegExp = this.apiNameRegExp;
      for (const iface of this.interfaceNames) {
        if (apiCatalog[iface]) {
          apiCatalog[iface] = apiCatalog[iface]
              .filter(apiName => !apiNameRegExp.test(apiName));
        }
      }
    }
  ]
});

foam.CLASS({
  name: 'AddApiProcessor',
  package: 'org.chromium.apis.web',
  implements: ['org.chromium.apis.web.ApiCatalogPostProcessor'],

  documentation: `Add a list of property (or method) names to an interface.`,

  properties: [
    {
      class: 'String',
      name: 'interfaceName',
      documentation: `Name of interface to be extended.`,
      required: true,
    },
    {
      class: 'StringArray',
      name: 'apiNames',
      documentation: `API method or property names be added to interface.`,
      required: true,
    },
  ],

  methods: [
    function postProcess(apiCatalog, og) {
      this.validate();

      if (!apiCatalog[this.interfaceName]) {
        apiCatalog[this.interfaceName] = [];
      }
      apiCatalog[this.interfaceName] = apiCatalog[this.interfaceName]
          .concat(this.apiNames);
    }
  ]
});

foam.CLASS({
  name: 'CopyToInterfaceProcessor',
  package: 'org.chromium.apis.web',
  implements: ['org.chromium.apis.web.ApiCatalogPostProcessor'],

  documentation: 'Copy APIs from one interface to another.',

  properties: [
    {
      class: 'String',
      name: 'fromInterfaceName',
      documentation: `Old name for interface.`,
      required: true,
    },
    {
      class: 'String',
      name: 'toInterfaceName',
      documentation: `New name for interface.`,
      required: true,
    },
  ],

  methods: [
    function postProcess(apiCatalog, og) {
      this.validate();

      if (!apiCatalog.hasOwnProperty(this.fromInterfaceName))
        return;

      const fromArray = apiCatalog[this.fromInterfaceName];
      const toArray = apiCatalog[this.toInterfaceName] || [];
      apiCatalog[this.toInterfaceName] = toArray
        .concat(fromArray.filter(api => !toArray.includes(api)));
    },
  ],
});

foam.CLASS({
  name: 'CopyToPrototypeProcessor',
  package: 'org.chromium.apis.web',
  implements: ['org.chromium.apis.web.ApiCatalogPostProcessor'],

  documentation: `Copy APIs associated with global <fromInterfaceName> into the
      interface associated with its prototype.

      E.g., With window.Foo = {
        prototype: {
          __proto__: window.Bar.prototype,
          baz: function() { ... }
        },
      }, copy existence of Foo's baz method to Bar in apiCatalog.`,

  properties: [
    {
      class: 'String',
      name: 'fromInterfaceName',
      documentation: `Name of interface constructor to copy from.`,
      required: true,
    },
  ],

  methods: [
    function postProcess(apiCatalog, og) {
      this.validate();

      // Allow post-processor to run on object graphs that don't define the
      // input interface.
      const fromId = og.lookup(this.fromInterfaceName);
      if (!fromId) return;
      // Prototype is either:
      // (1) <fromId-as-function>.prototype, OR
      // (2) <fromId-as-object>.
      const fromProtoId = og.getFunctionName(fromId) ?
          og.lookup('prototype', fromId) : fromId;
      if (!fromProtoId) return;
      const toProtoId = og.getPrototype(fromProtoId);
      if (!toProtoId) return;

      const interfaceNames = og.getKeys(toProtoId)
            .filter(key => key.endsWith('.prototype'))
            .map(key => {
              const parts = key.split('.');
              return parts[parts.length - 2];
            });
      foam.assert(interfaceNames.length > 0,
                  `Expected CopyToPrototypeProcessor prototype to have string
                       key(s) of ...<prototype ctor name>.prototype.`);

      const newAPIs = apiCatalog[this.fromInterfaceName];
      for (const interfaceName of interfaceNames) {
        const existingAPIs = apiCatalog[interfaceName] || [];
        apiCatalog[interfaceName] = existingAPIs
            .concat(newAPIs.filter(apiName => !existingAPIs.includes(apiName)));
      }
    },
  ],
});

foam.CLASS({
  name: 'RemoveObjectGraphRootProcessor',
  package: 'org.chromium.apis.web',
  implements: ['org.chromium.apis.web.ApiCatalogPostProcessor'],

  documentation: 'Remove interfaces that refer to the object graph root.',

  methods: [
    function postProcess(apiCatalog, og) {
      const root = og.getRoot();
      const globalNames = og.getObjectKeys(root);
      const globalObjectNames = globalNames
          .filter(name => og.lookup(name) === root);
      for (const name of globalObjectNames) {
        if (apiCatalog.hasOwnProperty(name)) delete apiCatalog[name];
      }
    },
  ],
});
