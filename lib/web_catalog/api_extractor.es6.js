// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('./post_processors.es6.js');

foam.INTERFACE({
  package: 'org.chromium.apis.web',
  name: 'ObjectGraphApiMapper',

  documentation: `Interface for storing ObjectGraph => ApiCatalog mapping
      metadata.`,

  methods: [
    {
      name: 'map',
      documentation: `Store the source (sourceId) an
          ObjectGraph-id => [apiNames] mapping. This captures "where an
          ApiCatalog API came from".`,
      args: [
        {
          name: 'interfaceId',
          documentation: `The ObjectGraph id of the interface constructor (or
              library object that describes the interface.`,
          typeName: 'Int',
        },
        {
          name: 'apiNames',
          documentation: `Names of APIs on the interface that are to be
              associated with "sourceId".`,
          typeName: 'StringArray',
        },
        {
          name: 'sourceId',
          documentation: `The ObjectGraph id of the interface prototype where
              "apiNames" were found when associating them with "interfaceId"'s
              interface.`,
          typeName: 'Int',
        },
      ],
      code: function(interfaceId, apiNames, sourceId) {},
    },
    {
      name: 'getObjectGraph',
      documentation: 'Get the ObjectGraph that owns ids in this mapping.',
      returns: 'ObjectGraph',
      code: function() {},
    },
    {
      name: 'getMap',
      documentation: `Get a map of of all known:

          Interface ObjectGraph id => API name => Source ObjectGraph id

          associations.`,
      returns: 'JSON',
      code: function() {},
    },
  ],
});

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'NullObjectGraphApiMapper',
  implements: ['org.chromium.apis.web.ObjectGraphApiMapper'],

  documentation: `No-op implementation of ObjectGraphApiMapper.`,

  properties: [
    ['objectGraph_', {}],
    ['map_', {}],
  ],

  methods: [
    function map(interfaceId, apiNames, sourceId) {},
    function getObjectGraph() { return this.objectGraph_; },
    function getMap() { return this.map_; },
  ],
});

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'DebugObjectGraphApiMapper',
  implements: ['org.chromium.apis.web.ObjectGraphApiMapper'],

  documentation: `Minimal implementation of ObjectGraphApiMapper that returns
      accurate data from all three interface functions.`,

  properties: [
    {
      name: 'objectGraph_',
      required: true,
    },
    ['map_', {}],
  ],

  methods: [
    function map(interfaceId, apiNames, sourceId) {
      let map = this.map_;
      const interfaceKey = interfaceId.toString();
      map[interfaceKey] = this.hap_(map, interfaceKey) ? map[interfaceId] : {};
      for (const apiName of apiNames) {
        map[interfaceKey][apiName] = sourceId;
      }
    },
    function getObjectGraph() {
      this.validate();
      return this.objectGraph_;
    },
    function getMap() {
      this.validate();
      return this.map_;
    },
    function hap_(object, propertyName) {
      return Object.prototype.hasOwnProperty.call(object, propertyName);
    },
  ],
});

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'ApiExtractor',

  documentation: `A component that extracts Interface/API pairs from a
      JavaScript object graph rooted at the global object. This implementation
      has several configuration properties that affect the behaviour of the main
      extraction algorithm.`,

  requires: [
    'org.chromium.apis.web.CopyToInterfaceProcessor',
    'org.chromium.apis.web.CopyToPrototypeProcessor',
    'org.chromium.apis.web.NullObjectGraphApiMapper',
    'org.chromium.apis.web.RemoveApisProcessor',
    'org.chromium.apis.web.RemoveInterfacesProcessor',
    'org.chromium.apis.web.RemoveObjectGraphRootProcessor',
  ],

  properties: [
    {
      class: 'Boolean',
      name: 'functionNamesFromGraphPaths',
      documentation: `Deduce function names from object graph paths that look
          like "...<Name>.prototype".`,
      value: true,
    },
    {
      class: 'Boolean',
      name: 'classNamesFromConstructorProperty',
      documentation: `Deduce class names the "constructor" property. Not that
          when a constructor is found, other means of deducing class names will
          not run.`,
      value: true,
    },
    {
      class: 'Boolean',
      name: 'classNamesFromGraphPaths',
      documentation: `Deduce class names from object graph paths that look
          like "...<Name>.prototype".`,
      value: true,
    },
    {
      class: 'Boolean',
      name: 'classNamesFromToString',
      documentation: `Deduce class names from object toString() calls that
          yielded "[object SomeClass]" or "[object SomeClassConstructor]" or
          "[object SomeClassPrototype]".`,
      value: true,
    },
    {
      class: 'Array',
      of: 'String',
      name: 'constantTypes',
      documentation: `Properties of these types with "value" property metadata
        are filtered if "retainConstantMembers" is false.`,
      factory: function() {
        return ['boolean', 'number', 'string'];
      },
    },
    {
      class: 'Boolean',
      name: 'retainConstantMembers',
      documentation: `Whether or not to retain non-writable properties of types
          in "constantTypes".`,
    },
    {
      name: 'blacklistInterfaces',
      documentation: `Blacklisted interfaces that may be visited in case they
          used for post-processing, but will not be stored.`,
      class: 'StringArray',
      factory: function() {
        return [
          // Stated at https://bugzilla.mozilla.org/show_bug.cgi?id=1290786#c6,
          // CSS2Properties are known bugs for some versions in Firefox.
          // We probably want to exclude them.
          'CSS2Properties',
          // "window" interface stored on "Window" instead. After copying to
          // "Window", remove "window".
          'window',
        ];
      },
    },
    {
      class: 'FObjectProperty',
      of: 'org.chromium.apis.web.ObjectGraphApiMapper',
      documentation: `The ObjectGraphApiMapper implementation to use for
          capturing extraction metadata. Default to no-op implementation.`,
      name: 'objectGraphApiMapper',
      factory: function() { return this.NullObjectGraphApiMapper.create(); },
    },
    {
      class: 'FObjectArray',
      of: 'org.chromium.apis.web.ApiCatalogPostProcessor',
      name: 'postProcessors',
      factory: function() {
        return [
          // Copy data around before (potentially) removing data that should be
          // copied.
          //
          // Copy processors:
          this.CopyToPrototypeProcessor.create({
            fromInterfaceName: 'CSS2Properties',
            confluenceIssueURL: 'https://github.com/GoogleChrome/confluence/issues/78',
            browserBugURL: 'https://bugzilla.mozilla.org/show_bug.cgi?id=1290786',
          }),
          // Keep one copy of global object APIs under "Window".
          this.CopyToInterfaceProcessor.create({
            fromInterfaceName: 'window',
            toInterfaceName: 'Window',
          }),

          // Remove processors:
          this.RemoveInterfacesProcessor.create({
            interfaceNames: this.blacklistInterfaces,
          }),
          this.RemoveApisProcessor.create({
            interfaceNames: ['CSSStyleDeclaration'],
            apiNameRegExp: /[-]/,
            confluenceIssueURL: '',
            specURL: 'https://drafts.csswg.org/cssom/#dom-cssstyledeclaration-dashed-attribute',
            otherURLs: [
              'https://github.com/GoogleChrome/confluence/issues/174',
              'https://github.com/w3c/csswg-drafts/issues/1089',
            ],
          }),
          // Remove all (other-than-"Window" above) copies of global object
          // APIs.
          this.RemoveObjectGraphRootProcessor.create(),
        ];
      },
    },
    {
      name: 'fns_',
      documentation: `<Ctor ObjectGraph id> => <interface names>`,
      factory: function() { return {}; },
    },
    {
      name: 'protos_',
      documentation: `<Ctor.prototype ObjectGraph id> =>
                          [<Ctor ObjectGraph ids>]`,
      factory: function() { return {}; },
    },
    {
      name: 'libs_',
      documentation: `<Library object ObjectGraph id> => <interface names>`,
      factory: function() { return {}; },
    },
    {
      name: 'apis_',
      documentation: `<Ctor ObjectGraph id> => <API names>`,
      factory: function() { return {}; },
    },
    {
      class: 'Int',
      documentation: `ObjectGraphId of "Object.prototype".`,
      name: 'objectDotPrototype_',
    },
    {
      class: 'Int',
      documentation: `ObjectGraphId of "Function.prototype".`,
      name: 'functionDotPrototype_',
    },
  ],

  methods: [
    {
      name: 'extractWebCatalog',
      documentation: `This function reads an object graph and produce
          a web catalog JSON.`,
      args: [
        {
          name: 'og',
          documentation: `The object graph where the
            Interface and API extracts from`,
          typeName: 'ObjectGraph',
        },
      ],
      code: function(og) {
        this.objectDotPrototype_ = og.lookup('Object.prototype');
        this.functionDotPrototype_ = og.lookup('Function.prototype');

        // Gather functions.
        const allOgIds = og.getAllIds();
        for (const id of allOgIds) {
          if (this.isFunctionLike_(og, id)) {
            const proto = og.lookup('prototype', id);
            const names = this.getFunctionNames_(og, id);
            let protos = this.protos_[proto] =
                  this.protos_.hasOwnProperty(proto) ? this.protos_[proto] : [];
            protos.push(id);
            this.fns_[id] = [];
            for (const name of names) {
              this.fns_[id].push(name);
            }
          }
        }

        // Gather "global libraries".
        const root = og.getRoot();
        const globalNames = og.getObjectKeys(root);
        for (const name of globalNames) {
          const id = og.lookup(name);
          if (!this.isFunctionLike_(og, id)) {
            let names = this.getClassNames_(og, id);
            if (!names.includes(name)) names.push(name);
            this.libs_[id] = this.apisConcat_((this.libs_[id] || []), names);
          }
        }

        // Gather APIs from libraries function/prototype pairs.
        for (const id of allOgIds) {
          const isCtor = this.fns_.hasOwnProperty(id);
          const isLib = this.libs_.hasOwnProperty(id);
          const isCtorProto = this.protos_.hasOwnProperty(id);

          foam.assert(!((isCtor && isLib) || (isCtor && isCtorProto) ||
                        (isLib && isCtorProto)),
                      'ApiExtractor: Object with multiple personalities');

          // Functions are interfaces, not prototypes. Just store properties and
          // carry on.
          if (isCtor) {
            this.storeAPIsFromCtorOrLib_(og, id);
          } else if (isLib) {
            this.storeAPIsFromCtorOrLib_(og, id);
            // Treat library as interface where Ctor = Ctor.prototype.
            const ctorId = id;
            const ctorNames = this.libs_[id];
            let protoId = id;
            let protoNames = this.getClassNames_(og, protoId);
            this.storeProtoAPIsForCtor_(og, ctorId, ctorNames, protoId,
                                        protoNames);
          } else if (isCtorProto) {
            // This is a "Foo.prototype", or a library object. Store its
            // properties under "Foo", and pull up any of its prototype's
            // properties, so long as those prototypes are not some
            // "Bar.prototype".
            const ctorIds = this.protos_[id];
            for (const ctorId of ctorIds) {
              const ctorNames = this.getFunctionNames_(og, ctorId);
              let protoId = id;
              let protoNames = this.getClassNames_(og, protoId);
              this.storeProtoAPIsForCtor_(og, ctorId, ctorNames, protoId,
                                          protoNames);
            }
          } // else: This is an instance or a primitive value.
        }

        // Copy APIs from instances down to closest interface prototype iff
        // the API does not exist further down in the prototype chain.
        for (const id of allOgIds) {
          const isInterfacePrototype =
              id => !og.isType(id) &&
                  id !== this.objectDotPrototype_ &&
                  id !== this.functionDotPrototype_;
          const isCtor = this.fns_.hasOwnProperty(id);
          const isCtorProto = this.protos_.hasOwnProperty(id);
          const isPrimitive = og.isType(id);
          if (!(isCtor || isCtorProto || isPrimitive)) {
            // This is some sort of "instance". Attempt to copy it (and its
            // prototype's) properties down to some "Foo.prototype" in its
            // prototype chain.
            let ctorIds = this.protos_[id];
            let protoId = id;
            let apis = [];
            let sources = [];
            while (isInterfacePrototype(protoId) && !ctorIds) {
              // Existing APIs are:
              // (1) APIs already found above "protoId" in prototype chain +
              // (2) APIs below "protoId" in prototype chain.
              const existingAPIs = this.apisConcat_(
                  apis, this.getAllProtosAPIs_(og, protoId));
              const newAPIs = this.apisFilter_(existingAPIs,
                                               this.getInstanceAPIs_(
                                                   og, protoId));
              sources.push({source: protoId, apis: newAPIs});
              apis = apis.concat(newAPIs);
              protoId = og.getPrototype(protoId);
              ctorIds = this.protos_[protoId];
            }

            // Store APIs when a ctor was found (and protoId did not bottom-out
            // to a primitive value).
            if (isInterfacePrototype(protoId) && ctorIds) {
              for (const source of sources) {
                for (const ctorId of ctorIds) {
                  this.objectGraphApiMapper.map(ctorId, source.apis,
                                                source.source);
                }
              }
              for (const ctorId of ctorIds) {
                this.storeAPIsFromArray_(og, ctorId, apis);
              }
            }
          } // else: This is a primitive value.
        }

        // Second pass at instances: Strip out APIs that were copied down
        // prototype chain during first pass.
        for (const id of allOgIds) {
          const isCtor = this.fns_.hasOwnProperty(id);
          const isCtorProto = this.protos_.hasOwnProperty(id);
          const isPrimitive = og.isType(id);
          if (!(isCtor || isCtorProto || isPrimitive)) {
            if (!this.apis_[id]) continue;
            const protosAPIs = this.getAllProtosAPIs_(og, id);
            this.apis_[id] = this.apis_[id]
                .filter(api => !protosAPIs.includes(api));
          }
        }

        let apiCatalog = {};
        for (const fnId in this.fns_) {
          if (this.apis_[fnId] && this.apis_[fnId].length > 0) {
            const names = this.fns_[fnId];
            for (const name of names) {
              apiCatalog[name] = this.apis_[fnId];
            }
          }
        }
        for (const libId in this.libs_) {
          if (this.apis_[libId] && this.apis_[libId].length > 0) {
            const names = this.libs_[libId];
            for (const name of names) {
              apiCatalog[name]  = apiCatalog.hasOwnProperty(name) ?
                this.apisConcat_(apiCatalog[name], this.apis_[libId]) :
                this.apis_[libId];
            }
          }
        }

        this.postProcess_(apiCatalog, og);

        return apiCatalog;
      },
    },
    function storeProtoAPIsForCtor_(og, ctorId, ctorNames, protoId,
                                    protoNames) {
      let nextProtoId = protoId;
      // Store APIs under "ctorId". Store APIs from  "protoId"'s prototype chain
      // so long as they do not appear to belong to a different class or
      // library. I.e., keep storing APIs on "ctorId" while:
      // (1) "nextProtoId" is an object (not a primitive type), and:
      //   (2a) "nextProtoId" is the first prototype (always belongs to
      //         "ctorId"), or
      //   (2b) "nextProtoId" is neither a library, nor some "Foo.prototype", or
      //   (2c) Names associated with "ctorId" and "nextProtoId" have some
      //        overlap.
      while (!og.isType(nextProtoId) &&
             (nextProtoId === protoId ||
              (!this.libs_.hasOwnProperty(nextProtoId) &&
               !this.protos_.hasOwnProperty(nextProtoId)) ||
              ctorNames.some(ctorName => protoNames.includes(ctorName)))) {
        this.storeAPIsFromProto_(og, ctorId, nextProtoId);
        nextProtoId = og.getPrototype(nextProtoId);
        protoNames = this.getClassNames_(og, nextProtoId);
      }
    },
    function getFunctionNames_(og, id) {
      const keys = og.getKeys(id);

      // Get names from end of path.to.function keys.
      const keyParts = keys.map(key => key.split('.'));
      const simpleNames = this.functionNamesFromGraphPaths ?
          keyParts.map(key => key[key.length - 1])
          .filter(name => !/^[+].*[+]$/.test(name))
          .filter(name => name !== 'prototype' && name !== '__proto__') : [];

      // Get names that look like
      // "CtorName.prototype.constructor" (with
      // "+constructor+") from object graph name mangling.
      const ctorNames = keyParts
              .filter(parts => parts[parts.length - 1] === '+constructor+' &&
                          parts[parts.length - 2] === 'prototype' &&
                          parts[parts.length - 3] &&
                          parts[parts.length - 3] !== '__proto__')
              .map(parts => parts[parts.length - 3]);

      let names = simpleNames
          .concat(ctorNames.filter(name => !simpleNames.includes(name)));

      // Get function name from object graph metadata.
      let nameFromOG = og.getFunctionName(id);
      if (nameFromOG && !names.includes(nameFromOG)) names.push(nameFromOG);

      return names;
    },
    function getClassNames_(og, protoId) {
      if (this.classNamesFromConstructorProperty &&
          og.getObjectKeys(protoId).includes('+constructor+')) {
        return this.getFunctionNames_(og, og.lookup('+constructor+', protoId));
      }

      return this.getCtorNamesFromProto_(og, protoId);
    },
    function getCtorNamesFromProto_(og, protoId) {
      let names = this.classNamesFromGraphPaths ? og.getKeys(protoId)
            .filter(key => key.endsWith('.prototype'))
            .map(key => {
              const parts = key.split('.');
              return parts[parts.length - 2];
            }) : [];
      if (this.classNamesFromToString) {
        let nameFromToString = this.getCtorNameFromToString_(og, protoId);
        if (nameFromToString && nameFromToString !== 'Object' &&
            !names.includes(nameFromToString)) {
          names.push(nameFromToString);
        }
      }
      return names;
    },
    function getCtorNameFromToString_(og, protoId) {
      let name = og.getToString(protoId);
      if (!name) return '';
      const match = name.match(/\[object ([A-Za-z_$][0-9A-Za-z_$]*)\]/);
      if (match === null) return '';
      name = match[1];
      if (name.endsWith('Prototype'))
        return name.substr(0, name.length - 'Prototype'.length);
      if (name.endsWith('Constructor'))
        return name.substr(0, name.length - 'Constructor'.length);
      return name;
    },
    function isFunctionLike_(og, id) {
      return (!og.isType(id)) &&
          og.getObjectKeys(id).includes('prototype');
    },
    function storeAPIsFromCtorOrLib_(og, ctorId) {
      let apis = this.apis_[ctorId] || [];
      // Do not copy properties belonging to Function API from ctors.
      const newAPIs = this.apisFilter_(
          apis, this.getClassAPIs_(og, ctorId).filter(api => ! [
            'arguments',
            'name',
            'length',
            'caller',
          ].includes(api)));

      // Store all valid API names from ctorId on ctorId's data.
      this.objectGraphApiMapper.map(ctorId, newAPIs, ctorId);
      this.apis_[ctorId] = apis.concat(newAPIs);
    },
    function storeAPIsFromProto_(og, ctorId, protoId) {
      // Gather APIs exposed in prototype chain beneath protoId.
      let existingAPIs = this.getAllProtosAPIs_(og, protoId);
      // Add to existingAPIs: APIs already registered to ctorId.
      const ctorAPIs = this.apis_[ctorId] = this.apis_[ctorId] || [];
      existingAPIs = this.apisConcat_(existingAPIs, ctorAPIs);

      // Add to ctorId: New APIs that:
      // (1) Are not exposed by some interface lower level than protoId's, AND
      // (2) Are not already on ctorId.
      const newAPIs = this.apisFilter_(existingAPIs,
                                       this.getClassAPIs_(og, protoId));
      this.objectGraphApiMapper.map(ctorId, newAPIs, protoId);
      this.apis_[ctorId] = ctorAPIs.concat(newAPIs);
    },
    function getAllProtosAPIs_(og, id) {
      let lowerProtoId = og.getPrototype(id);
      let apis = [];
      while (!og.isType(lowerProtoId)) {
        apis = this.apisConcat_(apis, this.getClassAPIs_(og, lowerProtoId));
        lowerProtoId = og.getPrototype(lowerProtoId);
      }
      return apis;
    },
    function storeAPIsFromArray_(og, ctorId, arr) {
      let apis = this.apis_[ctorId] = this.apis_[ctorId] || [];
      this.apis_[ctorId] = this.apisConcat_(apis, arr);
    },
    function getClassAPIs_(og, id) {
      if (id === this.objectDotPrototype_) {
        // Object.prototype: Store "marked-as-built-in" APIs with "+apiName+".
        return og.getObjectKeys(id)
            .filter(name => name !== 'prototype')
            .filter(this.filterConstantAPIs_.bind(this, og, id))
            .map(name => {
              const match = name.match(/^[+](.*)[+]$/);
              if (match === null) return name;
              return match[1];
            });
      }
      if (id === this.functionDotPrototype_) {
        // Function.prototype: Include non-writable APIs and "prototype" API.
        return og.getObjectKeys(id).filter(name => !/^[+].*[+]$/.test(name));
      }
      // Default: Exclude:
      // (1) object-graph'ified properties "+reservedName+",
      // (2) integers,
      // (3) the "prototype" property,
      // (4) uninteresting constants.
      return og.getObjectKeys(id)
          .filter(name => !/^[+].*[+]$/.test(name))
          .filter(name => !/^[0-9]+$/.test(name))
          .filter(name => name !== 'prototype')
          .filter(this.filterConstantAPIs_.bind(this, og, id));
    },
    function getInstanceAPIs_(og, id) {
      // Exclude object-graph'ified properties "+reservedName+" and integers.
      return og.getObjectKeys(id)
          .filter(name => !/^[+].*[+]$/.test(name))
          .filter(name => !/^[0-9]+$/.test(name));
    },
    function filterConstantAPIs_(og, id, name) {
      if (this.retainConstantMembers) return true;
      const nameId = og.lookup(name, id);
      return !(this.constantTypes.indexOf(og.getType(nameId)) !== -1 &&
               og.lookupMetaData(name, id).value === 1);
    },
    function apisConcat_(arr1, arr2) {
      // Filter upon concatenation.
      return arr1.concat(this.apisFilter_(arr1, arr2));
    },
    function apisFilter_(arr1, arr2) {
      // Filter for adding arr2 to arr1 of APIs: dedup string API names.
      return arr2.filter(apiName => !arr1.includes(apiName));
    },
    function postProcess_(apiCatalog, og) {
      for (let i = 0; i < this.postProcessors.length; i++) {
        this.postProcessors[i].postProcess(apiCatalog, og);
      }
    },
  ],
});
