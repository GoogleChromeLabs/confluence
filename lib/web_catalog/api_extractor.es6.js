// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('./post_processors.es6.js');

foam.CLASS({
  name: 'ApiExtractor',
  package: 'org.chromium.apis.web',

  documentation: 'Extract api catalog from a object graph.',

  requires: [
    'org.chromium.apis.web.RemoveInterfacesProcessor',
    'org.chromium.apis.web.RemoveApisProcessor',
    'org.chromium.apis.web.AddApiProcessor',
    'org.chromium.apis.web.CopyToPrototypeProcessor',
  ],

  properties: [
    {
      name: 'retainBuiltInMembers',
      class: 'Boolean',
      documentation: `When retainBuiltInMembers is true,
        built-in properties of function and object are returned.`,
      value: false,
    },
    {
      name: 'retainConstantMembers',
      class: 'Boolean',
      documentation: `When retainConstantMembers is false
        constant primitive properties are filtered.`,
      value: false,
    },
    {
      name: 'blacklistProperties',
      documentation: `Blacklisted properties that will not be visited or
        stored.`,
      class: 'StringArray',
      factory: function() {
        return [
          // toString is a default method of any objects.
          'toString',
          // Constructors have prototype but are not interfaces.
          'constructor',
        ];
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
        ];
      },
    },
    {
      name: 'constantTypes',
      class: 'StringArray',
      documentation: `Properties of these types with writable 0 are filtered
        if retainConstantMembers is false.`,
      factory: function() {
        return ['boolean', 'number', 'string'];
      },
    },
    {
      name: 'builtInFunctionProperties',
      class: 'StringArray',
      documentation: `Properties of Function.prototype are considered
        as boring properties which exists in all function objects.`,
      factory: function() {
        return ['prototype'];
      },
    },
    {
      name: 'builtInObjectProperties',
      class: 'StringArray',
      documentation: `Properties of Object.prototype are considered
        as boring properties which may exists in all objects.`,
    },
    {
      name: 'rootId',
      documentation: `The root's id of the object graph.`,
      value: null,
    },
    {
      name: 'objectProtoId',
      documentation: `Object.prototype's id.`,
      value: null,
    },
    {
      name: 'functionProtoId',
      documentation: `Function.prototype's id.`,
      value: null,
    },
    {
      name: 'objectId',
      documentation: `Object's id.`,
      value: null,
    },
    {
      name: 'functionId',
      documentation: `Function's id.`,
      value: null,
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

          // Add processors:
          this.AddApiProcessor.create({
            interfaceName: 'Object',
            apiNames: this.builtInObjectProperties,
          }),
          this.AddApiProcessor.create({
            interfaceName: 'Function',
            apiNames: this.builtInFunctionProperties,
          }),
        ];
      },
    }
  ],

  methods: [
    {
      name: 'getObjectProperties_',
      documentation: `A wrapper function to get properties with constant,
        non-own-properties filtered.`,
      args: [
        {
          documentation: `The object graph.`,
          name: 'og',
          typeName: 'ObjectGraph',
        },
        {
          documentation: `The id of object whose properties will be returned.`,
          name: 'id',
          typeName: 'Number',
        },
      ],
      returns: {
        documentation: `A string array contains properties
          of the object with given id`,
        typeName: 'StringArray',
      },
      code: function(og, id) {
        let keys = og.getObjectKeys(id);
        keys = keys.filter((key) => {
          let keyId = og.lookup(key, id);
          let meta = og.lookupMetaData(key, id);
          if (Object.keys(meta).length === 0) {
            // If meta contains no information, it means this property
            // is not own property but inherited from its prototype.
            // Inherited properties are not their APIs.
            return false;
          }
          if (!this.retainConstantMembers) {
            // When retainConstantMembers is turned off, filter out non-writable
            // properties whose type is listed in constantTypes.
            return this.constantTypes.indexOf(og.getType(keyId)) === -1
              || meta.writable !== 0;
          }
          return true;
        });
        return keys;
      },
    },
    {
      name: 'cleanUp_',
      documentation: `Remove +, $ chars in given string, return a new string.`,
      args: [
        {
          documentation: `The string that needs to be cleaned up.`,
          name: 'str',
          typeName: 'String',
        },
      ],
      returns: {
        documentation: `A string with special characters + and $ removed.`,
        typeName: 'String',
      },
      code: function(str) {
        return str.replace(/\+/g, '').replace(/\$/g, '');
      },
    },
    {
      name: 'isPositiveInt_',
      documentation: `Check if this string is a number.
        Purpose of this function is to check if a key is an index.
        Index properties of array or collections are not APIs.`,
      args: [
        {
          documentation: `The string which needs to be checked.`,
          name: 'str',
          typeName: 'String',
        },
      ],
      returns: {
        documentation: `Return a boolean if the given string
          is a positive number.`,
        typeName: 'Boolean',
      },
      code: function(str) {
        return /^\+?(0|[1-9]\d*)$/.test(str);
      },
    },
    {
      name: 'arrayMerge_',
      documentation: `Merge all given arrays into arr, no dupulicate allowed,
        boring and blacklisted properties are filtered.`,
      args: [
        {
          documentation: `The array will be merged into.`,
          name: 'arr',
          typeName: 'StringArray',
        },
        {
          documentation: `Array will be merged to the first array.`,
          name: 'StringArray',
        },
      ],
      code: function(arr) {
        for (let i = 1; i < arguments.length; i++) {
          for (let j = 0; j < arguments[i].length; j++) {
            let str = this.cleanUp_(arguments[i][j]);
            if (arr.indexOf(str) === -1 &&
              ((this.builtInObjectProperties.indexOf(str) === -1 &&
              this.builtInFunctionProperties.indexOf(str) === -1) ||
              // Check if the property is a index of an array or collection.
              // They are not APIs.
              this.retainBuiltInMembers) &&
              !this.isPositiveInt_(str)) {
              arr.push(str);
            }
          }
        }
      },
    },
    {
      name: 'setMinus_',
      documentation: `Return an array which is arr1 set minus arr2.`,
      args: [
        {
          documentation: `An array of API names.`,
          name: 'arr1',
          typeName: 'StringArray',
        },
        {
          documentation: `An array of API names.`,
          name: 'arr2',
          typeName: 'StringArray',
        },
      ],
      returns: {
        documentation: `An arry which is arr1 set minus arr2`,
        typeName: 'StringArray',
      },
      code: function(arr1, arr2) {
        let retArr = [];
        for (let i = 0; i < arr1.length; i++) {
          let str = this.cleanUp_(arr1[i]);
          let cleanUpArr2 = arr2.map((str) => this.cleanUp_(str));
          if (cleanUpArr2.indexOf(str) === -1) {
            // Add str to return Array if cannot find str in arr2.
            retArr.push(str);
          }
        }
        return retArr;
      },
    },
    {
      name: 'getFunctionBuiltInProperties_',
      documentation: `Get an array of properties from Function.prototype
        and push them to builtInFunctionProperties.`,
      args: [
        {
          documentation: `The object graph.`,
          name: 'og',
          typeName: 'ObjectGraph',
        },
      ],
      code: function(og) {
        this.builtInFunctionProperties = ['prototype'];
        this.functionId = og.lookup('Function', this.rootId);
        this.functionProtoId = og.lookup('prototype', this.functionId);
        let functionProp = og.getObjectKeys(this.functionProtoId);
        for (let i = 0; i < functionProp.length; i++) {
          this.builtInFunctionProperties.push(this.cleanUp_(functionProp[i]));
        }
      },
    },
    {
      name: 'getObjectBuiltInProperties_',
      documentation: `Get an array of properties from Object.prototype
        and push them to builtInObjectProperties.`,
      args: [
        {
          documentation: `The object graph.`,
          name: 'og',
          typeName: 'ObjectGraph',
        },
      ],
      code: function(og) {
        this.builtInObjectProperties = [];
        this.objectId = og.lookup('Object', this.rootId);
        this.objectProtoId = og.lookup('prototype', this.objectId);
        let objectProp = og.getObjectKeys(this.objectProtoId);
        for (let i = 0; i < objectProp.length; i++) {
          this.builtInObjectProperties.push(this.cleanUp_(objectProp[i]));
        }
      },
    },
    {
      name: 'getClassName_',
      documentation: `Search object id's prototype chain to find the the
        class it belongs to. The class is the prototype with a
        constructor property.`,
      args: [
        {
          documentation: `The id of object which it's class will be returned.`,
          name: 'id',
          typeName: 'Number',
        },
        {
          documentation: `The object graph.`,
          name: 'og',
          typeName: 'ObjectGraph',
        },
      ],
      returns: {
        documentation: 'The class of object with given id.',
        typeName: 'String',
      },
      code: function(id, og) {
        if (og.getType(id) !== 'object') return null;
        let ctorId = og.lookup('+constructor+', id);
        if (ctorId) {
          return og.getFunctionName(ctorId);
        }
        return this.getClassName_(og.getPrototype(id), og);
      },
    },
    {
      name: 'postProcess_',
      documentation: `Perform post-processing steps after initial extraction.`,
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
        for (let i = 0; i < this.postProcessors.length; i++) {
          this.postProcessors[i].postProcess(apiCatalog, og);
        }
      },
    },
    {
      name: 'extractAPI_',
      documentation: `extractAPI extracts API for interface with a given id.`,
      args: [
        {
          documentation: `The object graph.`,
          name: 'og',
          typeName: 'ObjectGraph',
        },
        {
          documentation: `The json stores web api for different interface.`,
          name: 'map',
          typeName: 'JSON',
        },
        {
          documentation: `The id currently visiting.`,
          name: 'id',
          typeName: 'Number',
        },
        {
          documentation: `The property name from its parent node.`,
          name: 'name',
          typeName: 'String',
        },
        {
          documentation: `A map of visted objects.`,
          name: 'visited',
          typeName: 'JSON',
        },
        {
          documentation: `It stores additional infomation about this object.`,
          name: 'opt',
          options: [
            {
              name: `firstLevel`,
              typeName: 'Boolean',
              documentation: `If the object is a first level object.
                First level objects are exists under window object.
                False if undefined.`,
            },
            {
              name: `proto`,
              typeName: 'Boolean',
              documentation: `If this object is a __proto__ of other object.
                False if undefined.`,
            },
          ],
        },
      ],
      code: function(og, map, id, name, visited, opt) {
        opt = opt || {};
        // First level means this object exists as window.X.
        let firstLevel = opt.firstLevel || false;
        // Proto means this object is a other object's __proto__.
        let proto = opt.proto || false;

        // Return if this object is already visited except
        // first level objects and proto objects.
        // For first level objects, we wish to have seperate interfaces
        // even they reference to the same object.
        // For __proto__ objects, if they are visited as a not first-level
        // or proto objects before, we will miss capturing them.
        if (visited.hasOwnProperty(id) && !firstLevel && !proto) return;

        // Return if this object is a primitive type.
        if (og.getType(id) !== 'object') return;

        // Skip root object, we don't want to visit window again.
        if (id === this.rootId) return;

        visited[id] = true;
        if (og.isFunction(id) && (proto || firstLevel)) {
          // This object is a function and it is either a __proto__
          // or on first level. Function which is not a __proto__ or first level
          // is not considered as an interface even extra properties exists.
          // Eg. chrome.webstore, chrome.runtime.

          // Skip if this function object is Function.prototype;
          if (id === this.functionProtoId) return;
          let prototypePropertyId = og.lookup('prototype', id);
          let ownProperties = this.getObjectProperties_(og, id);
          let prototypePropertyProps = [];
          // If function exists as a first level interface, use its property
          // name under window object as interface name, otherwise, use function
          // name. For example, window.mediaStream and window.webkitMediaStream
          // are same object with same function name. We do not want to lose
          // any of the two interfaces.
          let interfaceName = firstLevel ? name : og.getFunctionName(id);
          if (prototypePropertyId) {
            prototypePropertyProps = this.getObjectProperties_(
              og, prototypePropertyId);
          }
          this.arrayMerge_(ownProperties, prototypePropertyProps);
          let meaningfulProps = this.setMinus_(ownProperties,
            this.builtInFunctionProperties);
          if (meaningfulProps.length > 0) {
            // If this function object contains meaningful properties,
            // add them to our notion of interface members.
            if (!map[interfaceName]) map[interfaceName] = [];
            this.arrayMerge_(map[interfaceName],
              this.getObjectProperties_(og, id),
              this.getObjectProperties_(og, prototypePropertyId));
          }
          if (og.getPrototype(id)) {
            this.extractAPI_(og, map, og.getPrototype(id),
              `${name}.__proto__`, visited, {proto: true});
          }
        } else if (!og.isFunction(id)) {
          // Object is not a first level object or prototype function,
          // nor is it a function.
          let ownProperty = this.getObjectProperties_(og, id);
          if (this.setMinus_(ownProperty,
            this.builtInObjectProperties).length > 0) {
            let className = this.getClassName_(id, og);
            if ((!className || className === 'Object') && firstLevel) {
              // If object is at first level and its class name is "Object",
              // this object is considered as an object library,
              // such as MATH, console.
              let prototypePropertyId = og.lookup('prototype', id);
              // In Firefox, Edge, (maybe some other browser releases),
              // objects can have prototype properties.
              let prototypePropertyProps = [];
              if (prototypePropertyId) {
                prototypePropertyProps = this.getObjectProperties_(
                  og, prototypePropertyId);
              }
              if (!map[name]) map[name] = [];
              this.arrayMerge_(map[name],
                this.getObjectProperties_(og, id), prototypePropertyProps);
            } else {
              // This is not a object library, try to add its property to
              // it's prototype. For example, document.body.style contains
              // meaningful properties of CSSStyleDeclaration, then its
              // properties will be added to CSSStyleDeclaration interface.
              if (!className || className === 'Object') return;
              // Skip if its class is Object, or cannot find it's class name.
              // We don't want to add extra properties for Object.
              if (!map[className]) map[className] = [];
              this.arrayMerge_(map[className],
                this.getObjectProperties_(og, id));
            }
          }
          // Visit object.__proto__ if its __proto__ exists.
          // A __proto__ may also be a interface if it contains
          // meaningful properties.
          if (og.getPrototype(id)) {
            this.extractAPI_(og, map, og.getPrototype(id),
              `${name}.__proto__`, visited, {proto: true});
          }
        }

        // If the instance is not a __proto__ of other object,
        // visit its properties to find important interfaces.
        // For example, we need to visit document.body
        // to find properties for CSSStyleDeclaration under body.style.
        if (proto) return;

        // Visit object's properties.
        let propertiesIdMap = og.getPropertiesIds(id);
        for (let key in propertiesIdMap) {
          if (propertiesIdMap.hasOwnProperty(key)) {
            let cleanUpKey = this.cleanUp_(key);
            if (this.blacklistProperties.indexOf(cleanUpKey) === -1 &&
              cleanUpKey !== 'prototype') {
              // Visit object if it is not blacklisted or prototype.
              this.extractAPI_(og, map, propertiesIdMap[key],
                `${name}.${cleanUpKey}`, visited);
            }
          }
        }
      },
    },
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
        {
          name: 'opt',
          documentation: 'Config options.',
          typeName: 'JSON',
          options: [
            {
              name: `retainBuiltInMembers`,
              documentation: `When retainBuiltInMembers is true,
                built-in properties of function and object are returned`,
              typeName: 'Boolean',
            },
            {
              name: `retainConstantMembers`,
              documentation: `When retainConstantMembers is false
                constant primitive properties are filtered`,
              typeName: 'Boolean',
            },
          ],
        },
      ],
      code: function(og, opt) {
        opt = opt || {};
        this.retainBuiltInMembers = opt.retainBuiltInMembers ||
          this.retainBuiltInMembers;
        this.retainConstantMembers = opt.retainConstantMembers ||
          this.retainConstantMembers;
        const apiCatalog = {Window: []};
        // Read root ID, should be the id of window object.
        this.rootId = og.getRoot();
        // visited is a map keep track of visited objects.
        const visited = {rootId: true};
        this.getFunctionBuiltInProperties_(og);
        this.getObjectBuiltInProperties_(og);
        // Get all window.X as first level objects.
        const firstLevelObjects = og.getObjectKeys(this.rootId);
        for (let i = 0; i < firstLevelObjects.length; i++) {
          let interfaceObject = this.cleanUp_(firstLevelObjects[i]);
          // All objects under window object belongs to window interface.
          apiCatalog.Window.push(this.cleanUp_(interfaceObject));
          let objectId = og.lookup(interfaceObject, this.rootId);
          this.extractAPI_(og, apiCatalog, objectId, interfaceObject,
            visited, {firstLevel: true});
        }
        this.postProcess_(apiCatalog, og);
        return apiCatalog;
      },
    },
  ],
});
