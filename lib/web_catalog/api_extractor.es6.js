/**
 * @license
 * Copyright 2016 Google Inc. All Rights Reserved.
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

const objectGraph = require('object-graph-js').ObjectGraph;

var noise = false;     // True if base property like "caller", "length" will be returned.
var showConst = false; // True if constant number/string/boolean value will be returned.

var blacklistProperties = [
  'toString',    // Do not visit object's toString.
  'constructor',  // Do not visit constructor functions, they have prototype but not interface.
  'CSS2Properties' // These are known bugs in Firefox.
];

var blacklistFunctionAttr = ['prototype'];
// blacklistFunctionAttr is list of property of Function.prototype,
// prototype is not a part of function, but we also wish to blacklist it.

var blacklistObjectAttr = [];
// blacklistObjectAttr is list of property of Object.prototype.

var constType = ["boolean", "number", "string"];
// Primitive types which is constant will be blacklisted.

var rootId, ObjectId, FunctionId, objectId, functionId;

// A wrapper function of og.getObjectKeys, constant will
// be filtered if showConst is not turned on.
function getObjectProperties(og, id) {
  var keys = og.getObjectKeys(id);
  keys = keys.filter(key => {
    var keyId = og.lookup(key, id);
    var meta = og.lookupMetaData(key, id);
    if (Object.keys(meta).length === 0) {
      // If meta contains no information, it means this property
      // is inherited from its prototype. Inherited properties
      // will be added to thier prototype interface.
      return false;
    }
    if (!showConst) {
      // If showConst is not turned on, filter the result if
      // string it is premetive type and writable is false.
      return constType.indexOf(og.getType(keyId)) === -1 || meta.writable !== 0;
    }
    return true;
  });
  return keys;
}

// Remove all +, $ chars in given string, return the new string.
function cleanUp(str) {
  return str.replace(/\+/g, '').replace(/\$/g, '');
}

// Return if str is a positive interger.
function isPosotiveInt(str) {
  return /^\+?(0|[1-9]\d*)$/.test(str);
}

// Merge all given arrays into arr, no dupulicate allowed,
// blacklisted properties are filtered.
function arrayMerge(arr, opt) {
  opt = opt || {};

  for (var i = 1; i < arguments.length; i += 1) {
    for (var j = 0; j < arguments[i].length; j += 1) {
      var str = cleanUp(arguments[i][j]);
      if (arr.indexOf(str) === -1 &&
        ((blacklistObjectAttr.indexOf(str) === -1 &&
        blacklistFunctionAttr.indexOf(str) === -1) || noise) &&
        !isPosotiveInt(str)) {
        arr.push(str);
      }
    }
  }
}

// Return an array which is arr1 set minus arr2.
function setMinus(arr1, arr2) {
  var retArr = [];
  for (let i = 0; i < arr1.length; i += 1) {
    var str = cleanUp(arr1[i]);
    if (arr2.indexOf(str) === -1) {
      // Add str to restul Array if cannot find str in arr2.
      retArr.push(str);
    }
  }
  return retArr;
}

// Get list of property from Function.prototype
// and push to blacklistFunctionAttr;
function getFunctionBlacklist(og) {
  blacklistFunctionAttr = [];
  FunctionId = og.lookup('Function', rootId);
  functionId = og.lookup('prototype', FunctionId);
  var functionProp = og.getObjectKeys(functionId);
  for (var i = 0; i < functionProp.length; i += 1) {
    blacklistFunctionAttr.push(cleanUp(functionProp[i]));
  }
}

// Get list of property from Object.prototype
// and push to blacklistObjectAttr;
function getObjectBlacklist(og) {
  blacklistObjectAttr = [];
  ObjectId = og.lookup('Object', rootId);
  objectId = og.lookup('prototype', ObjectId);
  var objectProp = og.getObjectKeys(objectId);
  for (var i = 0; i < objectProp.length; i += 1) {
    blacklistObjectAttr.push(cleanUp(objectProp[i]));
  }
}

// Search object id's prototype chain to find the
// class it belongs to. The class is the prototype
// with a contructor property.
function getClassName(id, og) {
  var ctorId = og.lookup('+constructor+', id);
  if (ctorId) {
    return og.getFunctionName(ctorId);
  }
  return og.getFunctionName(og.getPrototype(id));
}

// Remove blacklist properties and add Function and Object's APIs.
function postProcess(apiCatalogs, og) {
  for (let i = 0; i < blacklistProperties.length; i += 1) {
    var prop = blacklistProperties[i];
    if (apiCatalogs[prop]) {
      delete apiCatalogs[prop];
    }
  }
  // Add function and obejct blacklisted property
  // to Function and Object interface.
  if (!apiCatalogs[og.getFunctionName(FunctionId)]) {
    apiCatalogs[og.getFunctionName(FunctionId)] = [];
  }
  apiCatalogs[og.getFunctionName(FunctionId)] =
    apiCatalogs[og.getFunctionName(FunctionId)].concat(blacklistFunctionAttr);
  if (!apiCatalogs[og.getFunctionName(ObjectId)]) {
    apiCatalogs[og.getFunctionName(ObjectId)] = [];
  }
  apiCatalogs[og.getFunctionName(ObjectId)] =
    apiCatalogs[og.getFunctionName(ObjectId)].concat(blacklistObjectAttr);
}

/** find API for interface of something with given id.
  @param {ObjectGraph} og - The obejct graph.
  @param {JSON} map - the json to store web api for interface
  @param {int} id - the id currently visiting
  @param {String} name - the property name from its parent node. (not necessarily interface name)
  @param {Array} visited - a list of visted properties
  @param {JSON} opt - know if this object is firstlevel or a __proto__ property.
*/
function extractAPI(og, map, id, name, visited, opt) {
  opt = opt || {};
  var firstLevel = opt.firstLevel || false;
  var proto = opt.proto || false;

  // Return if this object is already visited, except for first level objects.
  // Still visit it if this is a firstLevel object, since
  // two different firstlevel object can reference to one object.
  // We don't want to miss any of them.
  if (visited.indexOf(id) !== -1 && !firstLevel) return;
  // An object/function's Id must be greater than root's Id.
  // And we don't want to revisit window object.
  if (id <= rootId) return;
  visited.push(id);
  if (og.isFunction(id) && (proto || firstLevel)) {
    // This object is a function and it is either a __proto__ or on first level.
    // Function which is not a __protot__ or first level is not considered as an interface
    // eg: chrome.webstore, chrome.runtime.
    if (id === functionId) return; // Skip if function is Function.prototype;
    let prototypeId = og.lookup('prototype', id);
    let ownProperties = getObjectProperties(og, id);
    let prototypeProps = [];
    // If function exists as an first level interface, use its property name
    // under window object as interface name, otherwise, use function name.
    let interfaceName = firstLevel ? name : og.getFunctionName(id);
    if (prototypeId) prototypeProps = getObjectProperties(og, prototypeId);
    arrayMerge(ownProperties, prototypeProps);
    let meaningfulProps = setMinus(ownProperties, blacklistFunctionAttr);
    if (meaningfulProps.length > 0) {
      // If this function and its prototype contains meaning ful properties.
      if (!map[interfaceName]) map[interfaceName] = [];
      arrayMerge(map[interfaceName], getObjectProperties(og, id),
          getObjectProperties(og, prototypeId));
    }
    if (og.getPrototype(id)) {
      extractAPI(og, map, og.getPrototype(id), `${name}__proto__`, visited, {proto: true});
    }
  } else if (!og.isFunction(id)) {
    // Object is not a function.
    let ownProperty = getObjectProperties(og, id);
    if (setMinus(ownProperty, blacklistObjectAttr).length > 0) {
      let className = getClassName(id, og);
      if ((!className || className === "Object") && firstLevel) {
        // If object is at first level and its class name is "Object",
        // this object is considered as an object library.
        let prototypeId = og.lookup('prototype', id);
        // in Firefox Edge ..., objects can have prototype properties.
        let prototypeProps = [];
        if (prototypeId) {
          prototypeProps = getObjectProperties(og, prototypeId);
        }
        // if (!map[name]) map[name] = [INTERFACE];
        if (!map[name]) map[name] = [];
        arrayMerge(map[name], getObjectProperties(og, id), prototypeProps);
      } else {
        // This is not a object library, try to add its property to it's prototype.
        if (!className || className === "Object") return;
        // If its class is Object, or cannot find it's class name, return.
        // We don't want to add extra properties for Object.
        if (!map[className]) map[className] = [];
        arrayMerge(map[className], getObjectProperties(og, id));
      }
    }
    if (og.getPrototype(id)) {
      // Visit object __proto__ if its __proto__ exists.
      extractAPI(og, map, og.getPrototype(id), `${name}__proto__`, visited, {proto: true});
    }
  }

  // Dont visit __proto__'s properties.
  if (proto) return;

  // Only visit instances's properties.
  var propertiesIdMap = og.getPropertiesIds(id);
  var keys = Object.keys(propertiesIdMap);
  for (let i = 0; i < keys.length; i += 1) {
    var cleanUpKey = cleanUp(keys[i]);
    if (blacklistProperties.indexOf(cleanUpKey) === -1 && cleanUpKey !== 'prototype') {
      extractAPI(og, map, propertiesIdMap[keys[i]], `${name}.${cleanUpKey}`, visited);
    }
  }
}

// Functon getApiCatalog reads the name of file, not the absolute path.
module.exports = function getApiCatalog(og, opt) {
  opt = opt || {};
  noise = opt.noise || noise;
  showConst = opt.showConst || showConst;
  var apiCatalogs = {};
  rootId = og.getRoot(); // Read root ID, which is window object.
  var visited = [rootId];  // A list of visited functions.
  getFunctionBlacklist(og);
  getObjectBlacklist(og);
  var firstLevelObjects = og.getObjectKeys(rootId); // Get all window.X as first level objects.
  for (let i = 0; i < firstLevelObjects.length; i += 1) {
    let interfaceObject = cleanUp(firstLevelObjects[i]);
    let objectId = og.lookup(interfaceObject, rootId);
    extractAPI(og, apiCatalogs, objectId, interfaceObject, visited, {firstLevel: true});
  }

  // Window interface is a special case, add all exposed objects are in Window interface.
  if (!apiCatalogs.Window) apiCatalogs.Window = [];
  for (let i = 0; i < firstLevelObjects.length; i += 1) {
    let interfaceObject = firstLevelObjects[i];
    apiCatalogs.Window.push(cleanUp(interfaceObject));
  }
  postProcess(apiCatalogs, og);
  return apiCatalogs;
};
