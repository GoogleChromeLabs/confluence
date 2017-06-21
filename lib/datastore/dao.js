// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

// Protobuf represents dates differently than REST API.
foam.LIB({
  name: 'foam.Date',

  methods: [
    function toDatastoreValue(d) {
      var time = d.getTime();
      var millis = time % 1000;
      var seconds = Math.round(time / 1000).toString(10);
      var nanos = millis * 1000000;
      return {timestampValue: {seconds: seconds, nanos: nanos}};
    },
    function fromDatastoreValue(v) {
      var tv = v.timestampValue;
      var seconds = parseInt(tv.seconds);
      var nanos = tv.nanos;
      foam.assert( ! isNaN(seconds),
                   'Expected non-string Datastore timestampValue to contain: ' +
                   '{ seconds: "<seconds-since-epoch>", nanos: <nanos> }');

      return new Date((seconds * 1000) + Math.floor(nanos / 1000000));
    },
  ],
});

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'DatastoreDAO',
  extends: 'com.google.cloud.datastore.DatastoreDAO',

  documentation: `A DAO implementation for Datastore that uses gRPC via a
      @google-cloud/datastore dependency.`,

  imports: [
    'datastore', // Instance from require('@google-cloud/datastore)(<opts>).
    'error',
  ],

  properties: [
    {
      class: 'Function',
      name: 'request',
      documentation: `A local reference to the one library function we need:
          datastore.request().`,
      expression: function(datastore) {
        return datastore.request.bind(datastore);
      },
    },
  ],

  methods: [
    function sendRequest(protoMethod, reqOpts) {
      reqOpts = reqOpts || {};
      return new Promise(function(resolve, reject) {
        this.request(
          {service: 'Datastore', method: protoMethod},
          // REST and gRPC JS payloads are ALMOST identical. Fix peculiarities.
          this.hackProto_(protoMethod, reqOpts),
          function(error, response) {
            resolve({error: error, response: response});
          });
      }.bind(this));
    },
    {
      name: 'hackProto_',
      documentation: `There are some peculiar differences between the
          protos-in-JS and REST protocol (which this class inherits application
          logic from). This method fixes up these peculiarities before issuing
          a request.`,
      code: function(protoMethod, reqOpts) {
        // Sometimes the API expects the project ID explicitly on the request.
        // Always add it.
        reqOpts.projectId = reqOpts.projectId || this.projectId;

        // REST: Query.limit is a number, not a Value (which could correspond to
        // Int32Value).
        //
        // gRPC: Query.limit is Int32Value (not int32).
        // https://cloud.google.com/datastore/docs/reference/rpc/google.datastore.v1#google.datastore.v1.Query.FIELDS.google.protobuf.Int32Value.google.datastore.v1.Query.limit
        //
        // Change to Value with a different payload object; REST payload (and
        // its numeric values) are retained for select()-continuation logic.
        if (reqOpts.query && reqOpts.query.limit !== undefined &&
            protoMethod === 'runQuery') {
          reqOpts = Object.assign({}, reqOpts);
          var query = reqOpts.query;
          reqOpts.query = Object.assign(
              {}, query, {limit: {value: query.limit}});
        }

        return reqOpts;
      },
    },
  ],

  listeners: [
    function onResponse(name, errorResponse) {
      if (errorResponse.error) {
        this.error('gRPC Datastore DAO error on ' + name);
        throw errorResponse.error;
      }

      return errorResponse.response;
    },
  ],
});
