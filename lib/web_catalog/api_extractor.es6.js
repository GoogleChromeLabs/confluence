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
      let m = this.map_;
      const interfaceKey = interfaceId.toString();
      m[interfaceKey] = this.hap_(m, interfaceKey) ? m[interfaceId] : {};
      for (const apiName of apiNames) {
        m[interfaceKey][apiName] = sourceId;
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
    {
      name: 'hap_',
      documentation: `Invoke Object.prototype's implementation of
          hasOwnProperty(). This is necessary in cases where it may have been
          overridden to store interface information.`,
      code: function(object, propertyName) {
        return Object.prototype.hasOwnProperty.call(object, propertyName);
      },
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
  imports: ['warn'],

  properties: [
    {
      name: 'objectGraph',
      documentation: 'ObjectGraph from which APIs are extracted.',
      required: true,
      final: true,
    },
    {
      class: 'Boolean',
      name: 'functionNamesFromGraphPaths',
      documentation: `Deduce function names from object graph paths.`,
      value: true,
    },
    {
      class: 'Boolean',
      name: 'classNamesFromConstructorProperty',
      documentation: `Deduce class names the "constructor" property. Note that
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
          are filtered out.`,
      factory: function() {
        return ['boolean', 'number', 'string'];
      },
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
      documentation: `ObjectGraphId of "Function".`,
      name: 'function_',
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
    {
      class: 'Function',
      name: 'getAllProtosAPIs_',
      documentation: `Collect all APIs from prototype chain of "id", excluding
          "id"'s APIs.`,
      factory: function() {
        // Fresh memoizing cache for each instance of ApiExtractor.
        return foam.Function.memoize1(function(id) {
          const og = this.objectGraph;
          const lowerProtoId = og.getPrototype(id);
          if (og.isType(lowerProtoId)) return [];
          return this.dedupConcat_(this.getClassAPIs_(lowerProtoId),
                                   this.getAllProtosAPIs_(lowerProtoId));
        });
      },
    },
  ],

  methods: [
    {
      name: 'extractWebCatalog',
      documentation: `This function reads an object graph and produce
          a web catalog JSON.`,
      code: function() {
        const og = this.objectGraph;
        this.function_ = og.lookup('Function');
        this.objectDotPrototype_ = og.lookup('Object.prototype');
        this.functionDotPrototype_ = og.lookup('Function.prototype');

        // Gather functions.
        const allOgIds = og.getAllIds();
        for (const id of allOgIds) {
          if (this.isFunctionLike_(id)) {
            const ctorId = id;
            this.fns_[ctorId] = this.getFunctionNames_(ctorId);
          }
        }

        // Gather "global libraries".
        const root = og.getRoot();
        const globalNames = og.getObjectKeys(root);
        for (const name of globalNames) {
          const id = og.lookup(name);
          if ((!og.isType(id)) && (!this.isFunctionLike_(id))) {
            const libId = id;
            let names = this.getClassNames_(libId);
            if (!names.includes(name)) names.push(name);
            this.libs_[libId] = this.dedupConcat_(this.libs_[libId], names);
          }
        }

        // Gather prototypes for constructors and libraries.
        const ctors = Object.keys(this.fns_).map(str => parseInt(str));
        const libs = Object.keys(this.libs_).map(str => parseInt(str));
        // First pass: Store each "SomeConstructor.prototype" and "SomeLibrary".
        for (const ctorId of ctors) {
          const protoId = og.lookup('prototype', ctorId);
          this.storeProto_(protoId, ctorId);
        }
        for (const libId of libs) {
          // Treat library as interface where Ctor = Ctor.prototype.
          this.storeProto_(libId, libId);
        }
        // Second pass: Gather prototypes of "SomeConstructor.prototype" or
        // "SomeLibrary" that do not belong to "SomeOtherConstructor.protoype"
        // or "SomeOtherLibrary".
        for (const ctorId of ctors) {
          const ctorNames = this.fns_[ctorId];
          const protoId = og.lookup('prototype', ctorId);
          this.gatherProtosForCtor_(ctorId, ctorNames, protoId);
        }
        for (const libId of libs) {
          // Treat library as interface where Ctor = Ctor.prototype.
          const ctorId = libId;
          const ctorNames = this.libs_[libId];
          const protoId = libId;
          this.gatherProtosForCtor_(ctorId, ctorNames, protoId);
        }

        // Store APIs for constructors and libraries.
        for (const ctorId of ctors) {
          this.storeAPIsFromCtorOrLib_(ctorId);
        }
        for (const libId of libs) {
          this.storeAPIsFromCtorOrLib_(libId);
        }

        // Store APIs from constructor or library prototypes.
        const protos = Object.keys(this.protos_).map(str => parseInt(str));
        for (const protoId of protos) {
          for (const ctorOrLibId of this.protos_[protoId]) {
            this.storeAPIsFromProto_(ctorOrLibId, protoId);
          }
        }

        //
        // Deal with instances:
        //

        // Copy APIs from libraries and instances down to their first
        // API-containing prototype.
        const isInstanceOrLib = id => !(this.isBuiltIn_(id) ||
                                        ctors.includes(id) ||
                                        protos.includes(id));
        let instances = [];
        for (const id of allOgIds) {
          if (!isInstanceOrLib(id)) continue;
          const instanceOrLibId = id;
          this.copyInstanceOrLibAPIsToProtos_(instanceOrLibId);
          if (!libs.includes(instanceOrLibId)) instances.push(instanceOrLibId);
        }

        // Clear instance APIs that are now accounted for in their prototype
        // chain.
        for (const instanceId of instances) {
          this.clearInstanceProtoAPIs_(instanceId);
        }

        //
        // Build catalog from constructors and libraries.
        //

        let apiCatalog = {};
        let fnNameMap = new Map();
        for (const fnId in this.fns_) {
          if (this.apis_[fnId] && this.apis_[fnId].length > 0) {
            const names = this.fns_[fnId];
            for (const name of names) {
              if (fnNameMap.has(name)) {
                this.warn(`Merging duplicate interfaces named "${name}":
                              ${fnNameMap.get(name)
                                    .map(idStr => og.getShortestKey(idStr))
                                    .join(', ')},
                              and ${og.getShortestKey(fnId)}`);
                apiCatalog[name] = this.dedupConcat_(apiCatalog[name],
                                                          this.apis_[fnId]);
              } else {
                fnNameMap.set(name, []);
                apiCatalog[name] = this.apis_[fnId];
              }
              fnNameMap.get(name).push(fnId);
            }
          }
        }
        for (const libId in this.libs_) {
          if (this.apis_[libId] && this.apis_[libId].length > 0) {
            const names = this.libs_[libId];
            for (const name of names) {
              apiCatalog[name] = apiCatalog.hasOwnProperty(name) ?
                this.dedupConcat_(apiCatalog[name], this.apis_[libId]) :
                this.apis_[libId];
            }
          }
        }

        this.postProcess_(apiCatalog);

        return apiCatalog;
      },
    },
    function gatherProtosForCtor_(ctorId, ctorNames, protoId) {
      const og = this.objectGraph;
      const protoNames = this.getClassNames_(protoId);
      // Store APIs under "ctorId". Store APIs from  "protoId"'s prototype chain
      // so long as they do not appear to belong to a different class or
      // library. I.e., keep storing APIs on "ctorId" while:
      // (1) "nextProtoId" is an object (not a primitive type), and:
      //   (2a) "nextProtoId" is neither a library, nor some "Foo.prototype", or
      //   (2b) Names associated with "ctorId" and "nextProtoId" have some
      //        overlap.
      let nextProtoId = og.getPrototype(protoId);
      let nextProtoNames = this.getClassNames_(nextProtoId);
      while (!og.isType(nextProtoId) &&
             ((!this.libs_.hasOwnProperty(nextProtoId) &&
               !this.protos_.hasOwnProperty(nextProtoId)) ||
              ctorNames.some(ctorName => nextProtoNames.includes(ctorName)))) {
        this.storeProto_(nextProtoId, ctorId);
        nextProtoId = og.getPrototype(nextProtoId);
        nextProtoNames = this.getClassNames_(nextProtoId);
      }
    },
    function copyInstanceOrLibAPIsToProtos_(instanceOrLibId) {
      const og = this.objectGraph;
      // Attempt to copy APIs of "instanceOrLibId" (and its prototypes')
      // properties down to some "Foo.prototype" in its prototype chain.
      let ctorIds = this.protos_[instanceOrLibId];
      let protoId = instanceOrLibId;
      let apis = [];
      let sources = [];
      while ((!this.isBuiltIn_(protoId)) && !ctorIds) {
        // Existing APIs are:
        // (1) APIs already found above "protoId" in prototype chain +
        // (2) APIs below "protoId" in prototype chain.
        const existingAPIs = this.dedupConcat_(
            apis, this.getAllProtosAPIs_(protoId));
        const newAPIs = this.dedupFilter_(existingAPIs,
                                         this.getInstanceAPIs_(protoId));
        sources.push({source: protoId, apis: newAPIs});
        apis = apis.concat(newAPIs);
        protoId = og.getPrototype(protoId);
        ctorIds = this.protos_[protoId];
      }

      // Store APIs when a ctor was found (and protoId did not bottom-out
      // to a primitive value).
      if ((!this.isBuiltIn_(protoId)) && ctorIds) {
        for (const source of sources) {
          for (const ctorId of ctorIds) {
            this.objectGraphApiMapper.map(ctorId, source.apis,
                                          source.source);
          }
        }
        for (const ctorId of ctorIds) {
          this.storeAPIsFromArray_(ctorId, apis);
        }
      }
    },
    function clearInstanceProtoAPIs_(instanceId) {
      const og = this.objectGraph;
      if (!this.apis_[instanceId]) return;
      const protosAPIs = this.getAllProtosAPIs_(instanceId);
      this.apis_[instanceId] = this.apis_[instanceId]
          .filter(api => !protosAPIs.includes(api));
    },
    function getFunctionNames_(id) {
      const og = this.objectGraph;
      const keys = og.getKeys(id);
      let names = [];

      if (this.functionNamesFromGraphPaths) {
        // Get names from end of path.to.function keys.
        const keyParts = keys.map(key => key.split('.'));
        const simpleNames = keyParts.map(parts => parts[parts.length - 1])
            .filter(name => !/^[+].*[+]$/.test(name))
            .filter(name => name !== 'prototype' && name !== '__proto__');

        // Get names that look like
        // "CtorName.prototype.constructor" (with
        // "+constructor+") from object graph name mangling.
        const ctorNames = keyParts
            .filter(parts => parts[parts.length - 1] === '+constructor+' &&
                    parts[parts.length - 2] === 'prototype' &&
                    parts[parts.length - 3] &&
                    parts[parts.length - 3] !== '__proto__')
            .map(parts => parts[parts.length - 3]);

        names = this.dedupConcat_(this.dedupConcat_(names, simpleNames),
                                  ctorNames);
      }

      // Get function name from object graph metadata.
      let nameFromOG = og.getFunctionName(id);
      if (nameFromOG && !names.includes(nameFromOG)) names.push(nameFromOG);

      return names;
    },
    function getClassNames_(protoId) {
      const og = this.objectGraph;
      if (this.classNamesFromConstructorProperty &&
          og.getObjectKeys(protoId).includes('+constructor+')) {
        return this.getFunctionNames_(og.lookup('+constructor+', protoId));
      }

      return this.getCtorNamesFromProto_(protoId);
    },
    function getCtorNamesFromProto_(protoId) {
      const og = this.objectGraph;
      let names = this.classNamesFromGraphPaths ? og.getKeys(protoId)
            .filter(key => key.endsWith('.prototype'))
            .map(key => {
              const parts = key.split('.');
              return parts[parts.length - 2];
            }) : [];

      if (this.classNamesFromToString) {
        let nameFromToString = this.getCtorNameFromToString_(protoId);
        if (nameFromToString && nameFromToString !== 'Object' &&
            !names.includes(nameFromToString)) {
          names.push(nameFromToString);
        }
      }
      return names;
    },
    function getCtorNameFromToString_(protoId) {
      const og = this.objectGraph;
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
    function isFunctionLike_(id) {
      const og = this.objectGraph;
      return (!og.isType(id)) &&
          og.getObjectKeys(id).includes('prototype');
    },
    function storeAPIsFromCtorOrLib_(ctorId) {
      const og = this.objectGraph;
      let apis = this.apis_[ctorId] || [];
      // Do not copy properties belonging to Function API from ctors.
      const newAPIs = this.dedupFilter_(
          apis, this.getClassAPIs_(ctorId).filter(api => ![
            'arguments',
            'name',
            'length',
            'caller',
          ].includes(api)));

      // Store all valid API names from ctorId on ctorId's data.
      this.objectGraphApiMapper.map(ctorId, newAPIs, ctorId);
      this.apis_[ctorId] = apis.concat(newAPIs);
    },
    function storeAPIsFromProto_(ctorId, protoId) {
      const og = this.objectGraph;
      // Gather APIs exposed in prototype chain beneath protoId.
      let existingAPIs = this.getAllProtosAPIs_(protoId);
      // Add to existingAPIs: APIs already registered to ctorId.
      const ctorAPIs = this.apis_[ctorId] = this.apis_[ctorId] || [];
      existingAPIs = this.dedupConcat_(existingAPIs, ctorAPIs);

      // Add to ctorId: New APIs that:
      // (1) Are not exposed by some interface lower level than protoId's, AND
      // (2) Are not already on ctorId.
      const newAPIs = this.dedupFilter_(existingAPIs,
                                       this.getClassAPIs_(protoId));
      this.objectGraphApiMapper.map(ctorId, newAPIs, protoId);
      this.apis_[ctorId] = ctorAPIs.concat(newAPIs);
    },
    function storeAPIsFromArray_(ctorId, arr) {
      const og = this.objectGraph;
      let apis = this.apis_[ctorId] = this.apis_[ctorId] || [];
      this.apis_[ctorId] = this.dedupConcat_(apis, arr);
    },
    function storeProto_(protoId, ctorId) {
      const protoIdStr = protoId.toString();
      let ctorIds = this.protos_[protoIdStr] =
          this.protos_.hasOwnProperty(protoIdStr) ?
          this.protos_[protoIdStr] : [];
      if (!ctorIds.includes(ctorId)) ctorIds.push(ctorId);
    },
    function getClassAPIs_(id) {
      const og = this.objectGraph;
      if (id === this.objectDotPrototype_) {
        // Object.prototype: Store "marked-as-built-in" APIs with "+apiName+".
        return og.getObjectKeys(id)
            .filter(name => name !== 'prototype')
            .filter(this.filterConstantAPIs_.bind(this, id))
            .map(name => {
              const match = name.match(/^[+](.*)[+]$/);
              if (match === null) return name;
              return match[1];
            });
      }
      if (id === this.function_ || id === this.functionDotPrototype_) {
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
          .filter(this.filterConstantAPIs_.bind(this, id));
    },
    function getInstanceAPIs_(id) {
      const og = this.objectGraph;
      // Exclude object-graph'ified properties "+reservedName+" and integers.
      return og.getObjectKeys(id)
          .filter(name => !/^[+].*[+]$/.test(name))
          .filter(name => !/^[0-9]+$/.test(name));
    },
    function filterConstantAPIs_(id, name) {
      if (this.constantTypes.length === 0) return true;
      const og = this.objectGraph;
      const nameId = og.lookup(name, id);

      const type = og.getType(nameId);
      return !(this.constantTypes.includes(type) &&
               (og.lookupMetaData(name, id).value === 1 ||
                // If type is boolean, value of false causes "value" to be 0;
                // return true ignoring "value" metadata.
                type === 'boolean'));
    },
    function dedupConcat_(opt_arr1, arr2) {
      const arr1 = opt_arr1 || [];
      // Filter upon concatenation.
      return arr1.concat(this.dedupFilter_(arr1, arr2));
    },
    function dedupFilter_(arr1, arr2) {
      // Filter for adding arr2 to arr1 of APIs: dedup string API names.
      return arr2.filter(apiName => !arr1.includes(apiName));
    },
    function isBuiltIn_(id) {
      const og = this.objectGraph;
      return og.isType(id) || id === this.objectDotPrototype_ ||
          id === this.functionDotPrototype_;
    },
    function postProcess_(apiCatalog) {
      const og = this.objectGraph;
      for (let i = 0; i < this.postProcessors.length; i++) {
        this.postProcessors[i].postProcess(apiCatalog, og);
      }
    },
  ],
});
