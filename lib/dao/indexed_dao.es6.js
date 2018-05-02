// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'AbstractFacetedIndexed',

  documentation: `Abstract class for something that is faceted (stores a Class
      in "of") and indexed according to "propertIndexGroups" property.`,

  properties: [
    {
      class: 'Class',
      name: 'of',
    },
    {
      class: 'Array',
      name: 'propertyIndexGroups',
      documentation: `An array of arrays of properties. Each inner array
          a property index. Strings are adapted as property names of "of";
          strings not wrapped in an array are wrapped. E.g.,

          of=SomeClass;
          propertyIndexGroups=['a', ['b', 'c]] adapts to:
              [[SomeClass.A], [SomeClass.B, SomeClass.C]]`,
      adapt: function(_, nu) {
        return nu.map(
            maybeArr => (Array.isArray(maybeArr) ? maybeArr : [maybeArr])
              .map(strOrProp => foam.core.Property.isInstance(strOrProp) ?
                   strOrProp : this.of.getAxiomByName(strOrProp)));
      },
    },
  ],
});

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'MDAO',
  extends: 'foam.dao.MDAO',
  implements: ['org.chromium.apis.web.AbstractFacetedIndexed'],

  documentation: `Extension of MDAO that uses "propertyIndexGroups" to specify
      property indices.`,

  properties: [
    {
      name: 'promise',
      transient: true,
      factory: function() {
        throw new Error(`Concrete JsonDAO "${this.cls_.id}" without required
            promise factory`);
      },
    },
  ],

  methods: [
    function init() {
      this.SUPER();
      this.propertyIndexGroups
          .forEach(index => this.addPropertyIndex.apply(this, index));
    },
  ],
});

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'SerializableIndexedDAO',
  extends: 'foam.dao.ProxyDAO',
  implements: ['org.chromium.apis.web.AbstractFacetedIndexed'],

  documentation: `Base class for serializable, indexable DAO descriptions.`,

  properties: [
    {
      class: 'foam.dao.DAOProperty',
      name: 'delegate',
      documentation: 'Transient DAO that will fetch data when instantiated.',
      transient: true,
    },
  ],
});
