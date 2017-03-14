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
'use strict';

describe('API extractor', function() {
  let objectGraph = global.ObjectGraph;
  let extractor = org.chromium.apis.web.apiExtractor.create({});
  const types = {
    'boolean': 2,
    'exception': 7,
    'null': 6,
    'number': 3,
    'string': 4,
    'symbol': 5,
    'undefined': 1,
  };

  describe('getObjectProperties()', function() {
    let og = objectGraph.fromJSON({
      'data': {
        '10000': {
          'constantNumber': 3,
          'constantObject': 6,
          'nonOwnProperty': 3,
          'property': 3,
          'object': 7,
          'builtInProperty': 3,
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
          'builtInProperty': {
            writable: 1,
          },
        },
      },
      types,
    });
    let properties = extractor.getObjectProperties_(og, 10000);
    extractor.builtInObjectProperties = ['builtInProperty'];
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
    it('does not filter built-in properties', function() {
      expect(properties).toContain('builtInProperty');
    });
    // Need to test retaining constant members case separately.
    extractor.retainConstantMembers = true;
    let propertiesWithConst = extractor.getObjectProperties_(og, 10000);
    it('contains constant property if retainConstantMembers is true',
    function() {
      expect(propertiesWithConst).toContain('constantNumber');
    });
  });

  describe('cleanUp()', function() {
    it('removes all + and $ characters', function() {
      expect(extractor.cleanUp_('+toString+')).toBe('toString');
      expect(extractor.cleanUp_('$prototype$')).toBe('prototype');
    });
  });

  describe('isPositiveInt()', function() {
    it('correctly identifies a number string to be true', function() {
      expect(extractor.isPositiveInt_('0')).toBe(true);
      expect(extractor.isPositiveInt_('1')).toBe(true);
      expect(extractor.isPositiveInt_('2')).toBe(true);
      expect(extractor.isPositiveInt_('3')).toBe(true);
      expect(extractor.isPositiveInt_('10')).toBe(true);
      expect(extractor.isPositiveInt_('999')).toBe(true);
    });
    it('does not identify a non-number string to be false', function() {
      expect(extractor.isPositiveInt_('$1')).toBe(false);
      expect(extractor.isPositiveInt_('toString')).toBe(false);
    });
  });

  describe('arrayMerge()', function() {
    it('merges arrays into first array with no duplicates', function() {
      let array1 = ['a', 'b'];
      let array2 = ['a', 'c', 'd'];
      let array3 = ['b', 'd', 'f'];
      extractor.arrayMerge_(array1, array2, array3);
      expect(array1).toEqual(['a', 'b', 'c', 'd', 'f']);
    });
    it('filters out number(index) on merge', function() {
      let array1 = ['a', 'b'];
      let array2 = ['1', '2', 'd'];
      let array3 = ['3', '5', 'f'];
      extractor.arrayMerge_(array1, array2, array3);
      expect(array1).toEqual(['a', 'b', 'd', 'f']);
    });
    it('cleans up for strings on merge', function() {
      let array1 = ['a', 'b'];
      let array2 = ['$a$', '$c$'];
      let array3 = ['+c+', '+b+', '+f+'];
      extractor.arrayMerge_(array1, array2, array3);
      expect(array1).toEqual(['a', 'b', 'c', 'f']);
    });
    it('filters out built-in functions', function() {
      extractor.builtInObjectProperties = ['c', 'd'];
      let array1 = ['a', 'b'];
      let array2 = ['$a$', '$c$', 'd'];
      let array3 = ['+c+', '+b+', '+f+'];
      extractor.arrayMerge_(array1, array2, array3);
      expect(array1).toEqual(['a', 'b', 'f']);
    });
  });

  describe('setMinus()', function() {
    it('correctly finds set minus between two arrays', function() {
      let array1 = ['a', 'b', 'c'];
      let array2 = ['b', 'd'];
      expect(extractor.setMinus_(array1, array2)).toEqual(['a', 'c']);
      let array3 = ['a', 'b', 'c'];
      let array4 = ['b', 'd', 'a', 'c', 'e'];
      expect(extractor.setMinus_(array3, array4)).toEqual([]);
    });
    it('cleans up for string.', function() {
      let array1 = ['$a$', '$b$', '$c$', '+d+'];
      let array2 = ['b', 'd'];
      expect(extractor.setMinus_(array1, array2)).toEqual(['a', 'c']);
      let array3 = ['a', 'b', '$c$', '+e+'];
      let array4 = ['b', 'd', '$a$', 'c', 'e'];
      expect(extractor.setMinus_(array3, array4)).toEqual([]);
    });
  });

  describe('getFunctionBuiltInProperties()', function() {
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
    it('obtains Function built-in properties correctly', function() {
      extractor.getFunctionBuiltInProperties_(og);
      expect(extractor.builtInFunctionProperties.sort())
        .toEqual(['prototype', 'caller', 'arguments', 'length', 'toString']
        .sort());
    });
    it('cleans up string for properties', function() {
      extractor.getFunctionBuiltInProperties_(og);
      expect(extractor.builtInFunctionProperties).toContain('toString');
    });
  });

  describe('getObjectBuiltInProperties()', function() {
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
          '+constructor+': 6,
        },
      },
      types,
      'root': 10000,
    });
    it('obtains cleaned up Object built-in properties correctly', function() {
      extractor.getObjectBuiltInProperties_(og);
      expect(extractor.builtInObjectProperties.sort())
        .toEqual(['toString', 'valueOf', 'constructor']
        .sort());
    });
  });

  describe('getClassName()', function() {
    it("obtains class name of object's proto is its class", function() {
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
          '10030': 'hiddenClass',
        },
        types,
      });
      expect(extractor.getClassName_(10010, og)).toBe('hiddenClass');
    });
    it('obtains class name if the class is deep in the prototype chain',
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
          '10050': 'hiddenClass',
        },
        types,
      });
      expect(extractor.getClassName_(10010, og)).toBe('hiddenClass');
    });
    it('returns null if cannot find class', function() {
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
    it('returns null if id is not object id', function() {
      let og = objectGraph.fromJSON({
        'data': {
          '10000': {},
        },
        types,
      });
      expect(extractor.getClassName_(4, og)).toBeNull();
    });
  });

  describe('postProcess()', function() {
    let og = objectGraph.fromJSON({
      'functions': {
        '10001': 'Function',
        '10002': 'Object',
      },
      types,
    });
    extractor.builtInFunctionProperties = ['toString', 'caller', 'arguments'];
    extractor.builtInObjectProperties = ['toString', 'valueOf', 'constructor'];
    extractor.functionId = 10001;
    extractor.objectId = 10002;
    extractor.blacklistProperties = ['nonInterface'];
    let apiCatalogs = {
      'interface': ['API1', 'API2'],
      'nonInterface': ['API'],
    };
    extractor.postProcess_(apiCatalogs, og);
    it('filters blacklisted properties', function() {
      expect(apiCatalogs.nonInterface).toBeUndefined();
    });
    it('adds built-in properties for Function', function() {
      expect(apiCatalogs.Function)
        .toEqual(['toString', 'caller', 'arguments']);
    });
    it('adds built-in properties for Object', function() {
      expect(apiCatalogs.Object)
        .toEqual(['toString', 'valueOf', 'constructor']);
    });
    it('does not modify other interfaces', function() {
      expect(apiCatalogs.interface).toEqual(['API1', 'API2']);
    });
  });
});
