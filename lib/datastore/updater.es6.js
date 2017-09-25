// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('../confluence/set_ops.es6.js');

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'DatastoreUpdater',
  implements: ['org.chromium.mlang.Expressions'],

  imports: ['info'],

  methods: [
    function unversionData(versionedDAO, unversionedDAO) {
      return versionedDAO.select().then(sink => {
        const array = sink.array;
        this.info(`Unversioning ${array.length} ${versionedDAO.of.id} objects`);
        const cls = unversionedDAO.of;
        let promises = [];
        for (let i = 0; i < array.length; i++) {
          // Do not unversion deleted records.
          if (!array[i].deleted_)
            promises.push(unversionedDAO.put(cls.create(array[i])));
        }
        return Promise.all(promises).then(() => {
          this.info(`Unversioned ${array.length} ${versionedDAO.of.id} objects`);
        });
      });
    },
    function importData(importDAO, cacheDAO, syncDAO) {
      return Promise.all([
        this.putData_(importDAO, cacheDAO, syncDAO),
        this.removeData_(importDAO, cacheDAO, syncDAO),
      ]).then(() => syncDAO.sync()).then(() => {
        this.info(`Imported ${importDAO.of.id} to Datastore`);
      });
    },
    function putData_(srcDAO, cmpDAO, dstDAO) {
        this.info(`Computing data changes for ${srcDAO.of.id}`);
        const sink = foam.dao.ArraySink.create();
        return srcDAO.select(this.DIFF_ONLY(cmpDAO, sink))
            .then(() => {
              const array = sink.array;
              this.info(`Pushing ${array.length} ${srcDAO.of.id} to Datastore`);
              let promises = [];
              for (var i = 0; i < array.length; i++) {
                // Push versioned class object to SyncDAO: dstDAO.
                let datum = dstDAO.of.create(array[i]);
                promises.push(dstDAO.put(datum));
              }
              return Promise.all(promises).then(() => {
                const num = array.length;
                this.info(`Pushed ${num} ${srcDAO.of.id} to Datastore`);
              });
            });
    },
    function removeData_(srcDAO, cmpDAO, dstDAO) {
      const sink = foam.dao.ArraySink.create();

      // Quick sanity check against deleting entire database.
      return srcDAO.select(this.COUNT()).then(countSink => {
        foam.assert(countSink.value > 0,
                    'Datastore update should not empty entire database' +
                    ` (new data source for ${srcDAO.of.id} is empty)`);

        // Remove records in cmp, but not src.
        return cmpDAO.select(this.SET_MINUS(srcDAO, sink));
      }).then(() => {
        const array = sink.array;
        this.info(`Deleting ${array.length} ${srcDAO.of.id} from Datastore`);
        let promises = [];
        for (var i = 0; i < array.length; i++) {
          promises.push(dstDAO.remove(array[i]));
        }
        return Promise.all(promises).then(() => {
          this.info(`Deleted ${array.length} ${srcDAO.of.id} from Datastore`);
        });
      });
    },
  ],
});
