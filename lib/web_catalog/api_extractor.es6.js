/**
 * @license
 * Copyright 2017 Google Inc. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
'use strict';

foam.CLASS({
  name: 'apiExtractor',
  package: 'com.web.catalog',
  description: 'Extract api catalog from a object graph',
  properties: [
    {
      name: 'defaults',
      description: 'Default value of configuration',
      factory: function() {
        return {
          // When reatainBuildInMembers is true,
          // build-in properties of function and object
          // are returned as APIs.
          retainBuildInMembers: false,
          // When retainConstantMembers is false,
          // constan primitive properties are filtered.
          retainConstantMembers: false,
        };
      },
    },
    {
      name: 'blacklistProperties',
      description: 'Blacklisted properties will not visited or stored',
      factory: function() {
        return [
          // toString is a default method of any objects.
          'toString',
          // Constructors have prototype but are not interfaces.
          'constructor',
          // These are known bugs in Firefox.
          'CSS2Properties',
        ];
      },
    },
    {
      name: 'constantTypes',
      description: 'Properties of these types with writable 0 are filtered.' +
        'if defaults.retainConstantMembers is false',
      factory: function() {
        return ['boolean', 'number', 'string'];
      },
    },
    {
      name: 'buildInFunctionProperties',
      description: 'Properties of Function.prototype are considered' +
        'as boring properties which exists in all function objects.',
      factory: function() {
        return ['prototype'];
      },
    },
    {
      name: 'buildInObjectProperties',
      description: 'Properties of Object.prototype are considered' +
        'as boring properties which may exists in all objects.',
      factory: function() {
        return [];
      },
    },
    {
      name: 'metadata',
      description: 'Contains infomation of Object, Function and roots ids.',
      factory: function() {
        return {
          rootId: null,
          objectProtoId: null,
          functionProtoId: null,
          objectId: null,
          functionId: null,
        };
      },
    }
  ],
  methods: [
    {
      name: 'getObjectProperties_',
      description: 'A wrraper function to get properties with constant,' +
        'non-own-properties filtered.',
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
          if (!this.defaults.retainConstantMembers) {
            // If retainConstantMembers is not turned on, filter the result if
            // string it is primitive type and writable is false.
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
      description: 'Remove +, $ chars in given string, return a new string.',
      code: function(str) {
        return str.replace(/\+/g, '').replace(/\$/g, '');
      },
    },
    {
      name: 'isPosotiveInt_',
      description: 'check if this string is a number,' +
        'purpose of this function is to check if a key is an index,' +
        'index properties of array or collections are not APIs',
      code: function(str) {
        return /^\+?(0|[1-9]\d*)$/.test(str);
      },
    },
    {
      name: 'arrayMerge_',
      description: 'Merge all given arrays into arr, no dupulicate allowed,' +
        'boring and blacklisted properties are filtered',
      code: function(arr) {
        for (let i = 1; i < arguments.length; i++) {
          for (let j = 0; j < arguments[i].length; j++) {
            let str = this.cleanUp_(arguments[i][j]);
            if (arr.indexOf(str) === -1 &&
              ((this.buildInObjectProperties.indexOf(str) === -1 &&
              this.buildInFunctionProperties.indexOf(str) === -1) ||
              this.defaults.retainBuildInMembers) &&
              !this.isPosotiveInt_(str)) {
              arr.push(str);
            }
          }
        }
      },
    },
    {
      name: 'setMinus_',
      description: 'Return an array which is arr1 set minus arr2.',
      code: function(arr1, arr2) {
        let retArr = [];
        for (let i = 0; i < arr1.length; i++) {
          let str = this.cleanUp_(arr1[i]);
          let cleanUpArr2 = arr2.map((str) => this.cleanUp_(str));
          if (cleanUpArr2.indexOf(str) === -1) {
            // Add str to restul Array if cannot find str in arr2.
            retArr.push(str);
          }
        }
        return retArr;
      },
    },
    {
      name: 'getFunctionBuildInProperties_',
      description: 'Get list of properties from Function.prototype' +
        'and push to buildInFunctionProperties.',
      code: function getFunctionBlacklist(og) {
        this.buildInFunctionProperties = ['prototype'];
        this.metadata.functionId = og.lookup('Function', this.metadata.rootId);
        this.metadata.functionProtoId = og.lookup('prototype',
          this.metadata.functionId);
        let functionProp = og.getObjectKeys(this.metadata.functionProtoId);
        for (let i = 0; i < functionProp.length; i++) {
          this.buildInFunctionProperties.push(this.cleanUp_(functionProp[i]));
        }
      },
    },
    {
      name: 'getObjectBuildInProperties_',
      description: 'Get list of properties from Object.prototype' +
        'and push to buildInObjectProperties.',
      code: function(og) {
        this.buildInObjectProperties = [];
        this.metadata.objectId = og.lookup('Object', this.metadata.rootId);
        this.metadata.objectProtoId = og.lookup('prototype',
          this.metadata.objectId);
        let objectProp = og.getObjectKeys(this.metadata.objectProtoId);
        for (let i = 0; i < objectProp.length; i++) {
          this.buildInObjectProperties.push(this.cleanUp_(objectProp[i]));
        }
      },
    },
    {
      name: 'getClassName_',
      description: 'Search object id\'s prototype chain to find the the' +
        'class it belongs to. The class is the prototype with a' +
        'constructor property',
      code: function(id, og) {
        if (id === null || og.getType(id) !== 'object') return null;
        let ctorId = og.lookup('+constructor+', id);
        if (ctorId) {
          return og.getFunctionName(ctorId);
        }
        return this.getClassName_(og.getPrototype(id), og);
      },
    },
    {
      name: 'postProcess_',
      description: 'Remove blacklist properties and add' +
        'Function and Object\'s APIs.',
      code: function(apiCatalogs, og) {
        for (let i = 0; i < this.blacklistProperties.length; i++) {
          let prop = this.blacklistProperties[i];
          if (apiCatalogs[prop]) {
            delete apiCatalogs[prop];
          }
        }
        // Add function and object blacklisted property
        // to Function and Object interface.
        if (!apiCatalogs[og.getFunctionName(this.metadata.functionId)]) {
          apiCatalogs[og.getFunctionName(this.metadata.functionId)] = [];
        }
        apiCatalogs[og.getFunctionName(this.metadata.functionId)] =
          apiCatalogs[og.getFunctionName(this.metadata.functionId)]
            .concat(this.buildInFunctionProperties);
        if (!apiCatalogs[og.getFunctionName(this.metadata.objectId)]) {
          apiCatalogs[og.getFunctionName(this.metadata.objectId)] = [];
        }
        apiCatalogs[og.getFunctionName(this.metadata.objectId)] =
          apiCatalogs[og.getFunctionName(this.metadata.objectId)]
            .concat(this.buildInObjectProperties);
      },
    },
    {
      name: 'extractAPI_',
      description: 'extractAPI extract API for interface with a given id.',
      /*
        @param {ObjectGraph} og - The object graph.
        @param {JSON} map - the json to store web api for interface
        @param {int} id - the id currently visiting.
        @param {String} name - the property name from its parent node.
        @param {JSON} visited - a map of visted properties.
        @param {JSON} opt - store additional infomation about this object.
            opt.firstLevel: if the object is a first level,
                first level objects are exists under window object.
                False if undefined.
            opt.proto: if this object is a __proto__ of other object.
                False if undefined.

      */
      code: function(og, map, id, name, visited, opt) {
        opt = opt || {};
        // First level means this object exists as window.X.
        let firstLevel = opt.firstLevel || false;
        // Proto means this object is a other object's __proto__
        let proto = opt.proto || false;

        // Return if this object is already visited, except for
        // first level objects. Still visit it if this is a first
        // level object, since two different firstlevel object can
        // reference to one object. We don't want to miss any of them.
        if (visited.hasOwnProperty(id) && !firstLevel) return;

        // Return if this object is a primitive type.
        if (og.getType(id) !== 'object') return;

        // Skip root object, we don't want to visit window again.
        if (id === this.metadata.rootId) return;

        visited[id] = true;
        if (og.isFunction(id) && (proto || firstLevel)) {
          // This object is a function and it is either a __proto__
          // or on first level. Function which is not a __proto__ or first level
          // is not considered as an interface even extra properties exists.
          // Eg. chrome.webstore, chrome.runtime.

          // Skip if this function object is Function.prototype;
          if (id === this.metadata.functionProtoId) return;
          let prototypeId = og.lookup('prototype', id);
          let ownProperties = this.getObjectProperties_(og, id);
          let prototypeProps = [];
          // If function exists as a first level interface, use its property
          // name under window object as interface name, otherwise, use function
          // name. For example, window.mediaStream and window.webkitMediaStream
          // are same object with same function name. We do not want to lose
          // any of the two interfaces.
          let interfaceName = firstLevel ? name : og.getFunctionName(id);
          if (prototypeId) {
            prototypeProps = this.getObjectProperties_(og, prototypeId);
          }
          this.arrayMerge_(ownProperties, prototypeProps);
          let meaningfulProps = this.setMinus_(ownProperties,
            this.buildInFunctionProperties);
          if (meaningfulProps.length > 0) {
            // If this function object contains meaningful properties.
            if (!map[interfaceName]) map[interfaceName] = [];
            this.arrayMerge_(map[interfaceName],
              this.getObjectProperties_(og, id),
              this.getObjectProperties_(og, prototypeId));
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
            this.buildInObjectProperties).length > 0) {
            let className = this.getClassName_(id, og);
            if ((!className || className === 'Object') && firstLevel) {
              // If object is at first level and its class name is "Object",
              // this object is considered as an object library,
              // such as MATH, console.
              let prototypeId = og.lookup('prototype', id);
              // In Firefox, Edge, (maybe some other vendors or versions),
              // objects can have prototype properties.
              let prototypeProps = [];
              if (prototypeId) {
                prototypeProps = this.getObjectProperties_(og, prototypeId);
              }
              if (!map[name]) map[name] = [];
              this.arrayMerge_(map[name],
                this.getObjectProperties_(og, id), prototypeProps);
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
          if (og.getPrototype(id)) {
            // Visit object __proto__ if its __proto__ exists.
            this.extractAPI_(og, map, og.getPrototype(id),
              `${name}.__proto__`, visited, {proto: true});
          }
        }

        // Don't visit __proto__'s properties
        // since __proto__ does not contain instances.
        if (proto) return;

        // Visit object's properties.
        let propertiesIdMap = og.getPropertiesIds(id);
        let keys = Object.keys(propertiesIdMap);
        for (let i = 0; i < keys.length; i++) {
          let cleanUpKey = this.cleanUp_(keys[i]);
          if (this.blacklistProperties.indexOf(cleanUpKey) === -1 &&
            cleanUpKey !== 'prototype') {
            // Visit object if its is not blacklisted or prototype.
            this.extractAPI_(og, map, propertiesIdMap[keys[i]],
              `${name}.${cleanUpKey}`, visited);
          }
        }
      },
    },
    {
      name: 'extractWebCatalog',
      description: 'This function reads an object graph and produce' +
        'a web catalog JSON',
      code: function(og, opt) {
        opt = opt || {};
        this.defaults.retainBuildInMembers = opt.retainBuildInMembers ||
          this.defaults.retainBuildInMembers;
        this.defaults.retainConstantMembers = opt.retainConstantMembers ||
          this.defaults.retainConstantMembers;
        const apiCatalogs = {};
        // Read root ID, should be the id of window object.
        this.metadata.rootId = og.getRoot();
        // visited is a map keep track of visited objects.
        const visited = {rootId: true};
        this.getFunctionBuildInProperties_(og);
        this.getObjectBuildInProperties_(og);
        // Get all window.X as first level objects.
        const firstLevelObjects = og.getObjectKeys(this.metadata.rootId);
        for (let i = 0; i < firstLevelObjects.length; i++) {
          let interfaceObject = this.cleanUp_(firstLevelObjects[i]);
          let objectId = og.lookup(interfaceObject, this.metadata.rootId);
          this.extractAPI_(og, apiCatalogs, objectId, interfaceObject,
            visited, {firstLevel: true});
        }
        // Window interface is a special case,
        // add all exposed objects are in Window interface.
        if (!apiCatalogs.Window) apiCatalogs.Window = [];
        for (let i = 0; i < firstLevelObjects.length; i++) {
          let interfaceObject = firstLevelObjects[i];
          apiCatalogs.Window.push(this.cleanUp_(interfaceObject));
        }
        this.postProcess_(apiCatalogs, og);
        return apiCatalogs;
      },
    },
  ], // methods
}); // FOAM.CLASS
