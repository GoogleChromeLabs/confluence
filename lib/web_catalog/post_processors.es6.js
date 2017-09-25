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
      for (let i = 0; i < this.interfaceNames.length; i++) {
        let iface = this.interfaceNames[i];
        if (apiCatalog[iface]) {
          apiCatalog[iface] = apiCatalog[iface]
              .filter(apiName => apiName.match(apiNameRegExp) !== null);
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
  name: 'CopyToPrototypeProcessor',
  package: 'org.chromium.apis.web',
  implements: ['org.chromium.apis.web.ApiCatalogPostProcessor'],

  documentation: `Copy APIs associated with window.<fromInterfaceName> into the
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
      var fromId = og.lookup(`window.${this.fromInterfaceName}`);
      if (!fromId) return;
      var fromProtoId = og.lookup('prototype', fromId);
      if (!fromProtoId) return;
      var toProtoId = og.getPrototype(fromProtoId);
      if (!toProtoId) return;

      var keyParts = og.getShortestKey(toProtoId).split('.');
      foam.assert(
          keyParts[0] === 'window' && keyParts[2] === 'prototype' &&
              keyParts.length === 3,
          `Expected CopyToPrototypeProcessor prototype to have string key of
          window.<prototype ctor name>.prototype.`);
      var toName = keyParts[1];

      apiCatalog[toName] = apiCatalog[toName]
          .concat(apiCatalog[this.fromInterfaceName]);
    },
  ],
});
