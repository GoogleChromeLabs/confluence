/**
 * @license
 * Copyright 2017 Google Inc. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the 'License');
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an 'AS IS' BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

const objectGraph = require('object-graph-js').ObjectGraph;

require('../../../lib/web_catalog/api_extractor.es6.js');

let extractor = com.web.catalog.apiExtractor.create({});
const types = {
  'boolean': 2,
  'exception': 7,
  'null': 6,
  'number': 3,
  'string': 4,
  'symbol': 5,
  'undefined': 1,
};

describe('Method getObjectProperties', function() {
  let og = objectGraph.fromJSON({
    'data': {
      '10000': {
        'constantNumber': 3,
        'constantObject': 6,
        'nonOwnProperty': 3,
        'property': 3,
        'object': 7,
        'buildInProperty': 3,
      },
    },
    'metadata': {
      '10000': {
        'constantNumber': {
          writable: 0,
        },
        'constantObject': {
          writable: 0,
        },
        'property': {
          writable: 1,
        },
        'object': {
          writable: 1,
        },
        'buildInProperty': {
          writable: 1,
        },
      },
    },
    types,
  });
  let properties = extractor.getObjectProperties_(og, 10000);
  extractor.buildInObjectProperties = ['buildInProperty'];
  it('captures non-constant properties', function() {
    expect(properties).toContain('property');
  });
  it('captures object regardless if it is constant or not', function() {
    expect(properties).toContain('object');
  });
  it('excludes constant property', function() {
    expect(properties).not.toContain('constantNumber');
  });
  it('excludes non-own-property', function() {
    expect(properties).not.toContain('nonOwnProperty');
  });
  it('does not filter build-in properties', function() {
    expect(properties).toContain('buildInProperty');
  });
  // Turn on retainConstantMembers flag.
  extractor.defaults.retainConstantMembers = true;
  let propertiesWithConst = extractor.getObjectProperties_(og, 10000);
  it('contains constant property if retainConstantMembers is true', function() {
    expect(propertiesWithConst).toContain('constantNumber');
  });
});

describe('Method cleanUp', function() {
  it('remves all + and $ characters', function() {
    expect(extractor.cleanUp_('+toString+')).toEqual('toString');
    expect(extractor.cleanUp_('$prototype$')).toEqual('prototype');
  });
});

describe('Method isPosotiveInt', function() {
  it('correctly identify a number string to be true', function() {
    expect(extractor.isPosotiveInt_('0')).toEqual(true);
    expect(extractor.isPosotiveInt_('1')).toEqual(true);
    expect(extractor.isPosotiveInt_('2')).toEqual(true);
    expect(extractor.isPosotiveInt_('3')).toEqual(true);
    expect(extractor.isPosotiveInt_('10')).toEqual(true);
    expect(extractor.isPosotiveInt_('999')).toEqual(true);
  });
  it('will not identify a non-number string to be false', function() {
    expect(extractor.isPosotiveInt_('$1')).toEqual(false);
    expect(extractor.isPosotiveInt_('toString')).toEqual(false);
  });
});

describe('Method arrayMerge', function() {
  it('merges arrays into first array with no duplicates', function() {
    let array1 = ['a', 'b'];
    let array2 = ['a', 'c', 'd'];
    let array3 = ['b', 'd', 'f'];
    extractor.arrayMerge_(array1, array2, array3);
    expect(array1).toEqual(['a', 'b', 'c', 'd', 'f']);
  });
  it('filter out number(index) when merge', function() {
    let array1 = ['a', 'b'];
    let array2 = ['1', '2', 'd'];
    let array3 = ['3', '5', 'f'];
    extractor.arrayMerge_(array1, array2, array3);
    expect(array1).toEqual(['a', 'b', 'd', 'f']);
  });
  it('clean up for strings when merge', function() {
    let array1 = ['a', 'b'];
    let array2 = ['$a$', '$c$'];
    let array3 = ['+c+', '+b+', '+f+'];
    extractor.arrayMerge_(array1, array2, array3);
    expect(array1).toEqual(['a', 'b', 'c', 'f']);
  });
  it('filter out build-in functions', function() {
    extractor.buildInObjectProperties = ['c', 'd'];
    let array1 = ['a', 'b'];
    let array2 = ['$a$', '$c$', 'd'];
    let array3 = ['+c+', '+b+', '+f+'];
    extractor.arrayMerge_(array1, array2, array3);
    expect(array1).toEqual(['a', 'b', 'f']);
  });
});

describe('Method setMinus', function() {
  it('correctly find set minus between two arrays', function() {
    let array1 = ['a', 'b', 'c'];
    let array2 = ['b', 'd'];
    expect(extractor.setMinus_(array1, array2)).toEqual(['a', 'c']);
    let array3 = ['a', 'b', 'c'];
    let array4 = ['b', 'd', 'a', 'c', 'e'];
    expect(extractor.setMinus_(array3, array4)).toEqual([]);
  });
  it('clean up for string.', function() {
    let array1 = ['$a$', '$b$', '$c$', '+d+'];
    let array2 = ['b', 'd'];
    expect(extractor.setMinus_(array1, array2)).toEqual(['a', 'c']);
    let array3 = ['a', 'b', '$c$', '+e+'];
    let array4 = ['b', 'd', '$a$', 'c', 'e'];
    expect(extractor.setMinus_(array3, array4)).toEqual([]);
  });
});

describe('Method getFunctionBuildInProperties', function() {
  let og = objectGraph.fromJSON({
    'data': {
      '10000': {
        'Function': 10001,
      },
      '10001': {
        'prototype': 10002,
      },
      '10002': {
        'caller': 3,
        'arguments': 6,
        'length': 6,
        '+toString+': 6,
      },
    },
    types,
    'root': 10000,
  });
  it('obtain Function build-in properties correctly', function() {
    extractor.getFunctionBuildInProperties_(og);
    expect(extractor.buildInFunctionProperties.sort())
      .toEqual(['prototype', 'caller', 'arguments', 'length', 'toString']
      .sort());
  });
  it('clean up string for properties', function() {
    extractor.getFunctionBuildInProperties_(og);
    expect(extractor.buildInFunctionProperties).toContain('toString');
  });
});

describe('Method getObjectBuildInProperties', function() {
  let og = objectGraph.fromJSON({
    'data': {
      '10000': {
        'Object': 10001,
      },
      '10001': {
        'prototype': 10002,
      },
      '10002': {
        '+toString+': 3,
        '+valueOf+': 6,
        '+constructor': 6,
      },
    },
    types,
    'root': 10000,
  });
  it('obtain cleaned up Object build-in properties correctly', function() {
    extractor.getObjectBuildInProperties_(og);
    expect(extractor.buildInObjectProperties.sort())
      .toEqual(['toString', 'valueOf', 'constructor']
      .sort());
  });
});

describe('Method getClassName', function() {
  it('obtain class name of object\'s proto is its class', function() {
    let og = objectGraph.fromJSON({
      'data': {
        '10010': {
          'instanceProperty': 3,
        },
        '10020': {
          '+constructor+': 10030,
        },
      },
      'protos': {
        '10010': '10020',
      },
      'functions': {
        '10030': 'class',
      },
      types,
    });
    expect(extractor.getClassName_(10010, og)).toEqual('class');
  });
  it('obtain class name if it is the class is deep in the prototype chain',
  function() {
    let og = objectGraph.fromJSON({
      'data': {
        '10010': {
          'instanceProperty': 3,
        },
        '10020': {
        },
        '10030': {
        },
        '10040': {
          '+constructor+': 10050,
        },
      },
      'protos': {
        '10010': '10020',
        '10020': '10030',
        '10030': '10040',
      },
      'functions': {
        '10020': 'noClass',
        '10030': 'noClass',
        '10040': 'classPrototype',
        '10050': 'class',
      },
      types,
    });
    expect(extractor.getClassName_(10010, og)).toEqual('class');
  });
    it('return null if cannot find class', function() {
    let og = objectGraph.fromJSON({
      'data': {
        '10010': {
          'instanceProperty': 3,
        },
        '10020': {
        },
        '10030': {
        },
      },
      'protos': {
        '10010': '10020',
        '10020': '10030',
        '10030': '6',
      },
      'functions': {
        '10020': 'noClass',
        '10030': 'noClass',
      },
      types,
    });
    expect(extractor.getClassName_(10010, og)).toBeNull();
  });
});

describe('Method postProcess', function() {
  let og = objectGraph.fromJSON({
    'functions': {
      '10001': 'Function',
      '10002': 'Object',
    },
    types,
  });
  extractor.buildInFunctionProperties = ['toString', 'caller', 'arguments'];
  extractor.buildInObjectProperties = ['toString', 'valueOf', 'constructor'];
  extractor.metadata.functionId = 10001;
  extractor.metadata.objectId = 10002;
  extractor.blacklistProperties = ['nonInterface'];
  let apiCatalogs = {
    'interface': ['API1', 'API2'],
    'nonInterface': ['API'],
  };
  extractor.postProcess_(apiCatalogs, og);
  it('filtered blacklisted properties', function() {
    expect(apiCatalogs.nonInterface).toBeUndefined();
  });
  it('add build-in properties for Function', function() {
    expect(apiCatalogs.Function).toEqual(['toString', 'caller', 'arguments']);
  });
  it('add build-in properties for Object', function() {
    expect(apiCatalogs.Object).toEqual(['toString', 'valueOf', 'constructor']);
  });
  it('does not modify other interfaces', function() {
    expect(apiCatalogs.interface).toEqual(['API1', 'API2']);
  });
});
