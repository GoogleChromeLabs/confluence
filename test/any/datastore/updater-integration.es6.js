// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

describe('Datastore updater (set ops + versioned DAOs)', () => {
  var MDAO;
  var QuickSink;
  var SyncDAO;
  var VersionedSyncRecord;
  var VersionNoDAO;
  var DatastoreUpdater;
  var Item;
  var VersionedItem;
  var srcDAO;
  var cacheDAO;
  var dstDAO;
  var datastoreDAO;
  var updater;
  beforeEach(() => {
    MDAO = foam.lookup('foam.dao.MDAO');
    QuickSink = foam.lookup('foam.dao.QuickSink');
    SyncDAO = foam.lookup('foam.dao.SyncDAO');
    VersionedSyncRecord = foam.lookup('foam.dao.sync.VersionedSyncRecord');
    VersionNoDAO = foam.lookup('foam.dao.VersionNoDAO');
    DatastoreUpdater = foam.lookup('org.chromium.apis.web.DatastoreUpdater');

    foam.CLASS({
      package: 'org.chromium.mlang.sink.test',
      name: 'Item',

      properties: [
        {
          class: 'Int',
          name: 'id',
        },
        {
          name: 'data',
        },
      ],
    });

    Item = foam.lookup('org.chromium.mlang.sink.test.Item');
    VersionedItem = foam.lookup('foam.version.VersionedClassFactorySingleton')
        .create().get(Item);
    srcDAO = MDAO.create({of: Item});
    cacheDAO = MDAO.create({of: Item});
    datastoreDAO = MDAO.create({of: VersionedItem});
    dstDAO = SyncDAO.create({
      of: VersionedItem,
      delegate: MDAO.create({of: VersionedItem}),
      syncRecordDAO: MDAO.create({of: VersionedSyncRecord}),
      remoteDAO: VersionNoDAO.create({
        delegate: datastoreDAO,
      }),
      polling: true,
    });
    updater = DatastoreUpdater.create();
  });

  it('should only perform necessary put/remove updates', () => {
    srcDAO.put(Item.create({id: 1, data: 'src1'})); // Newly added.
    srcDAO.put(Item.create({id: 2, data: 'src2'})); // Updated.
    srcDAO.put(Item.create({id: 3, data: 'same3'})); // Not updated.
    // Bring back previously deleted record.
    srcDAO.put(Item.create({id: 4, data: 'same4deletedToUndeleted'}));
    // Bring back previously deleted record, and update data.
    srcDAO.put(Item.create({id: 5, data: 'different5deletedToUndeleted'}));

    // Promise returned from SyncDAO does not wait for underlying VersionNoDAO
    // to complete its operation. Use counters to support waiting for data to
    // arrive.
    let dstOpsStarted = 0;
    let dstOpsFinished = 0;
    function dstPut(o) {
      dstDAO.put(o);
      dstOpsStarted++;
    }
    function dstRemove(o) {
      dstDAO.put(o);
      dstOpsStarted++;
    }
    datastoreDAO.listen(QuickSink.create({
      putFn: function() { dstOpsFinished++; },
      removeFn: function() { dstOpsFinished++; },
    }));
    function dstWait() {
      return new Promise((resolve, reject) => {
        const intervalId = setInterval(() => {
          if (dstOpsStarted === dstOpsFinished) {
            clearInterval(intervalId);
            resolve();
          }
        }, 1);
      });
    }

    dstPut(VersionedItem.create({
      id: 2,
      data: 'dst2',
    }));
    dstPut(VersionedItem.create({
      id: 3,
      data: 'same3',
    }));
    dstPut(VersionedItem.create({
      id: 4,
      data: 'same4deletedToUndeleted',
    }));
    dstPut(VersionedItem.create({
      id: 5,
      data: 'unlike5deletedToUndeleted',
    }));
    dstPut(VersionedItem.create({
      id: 6,
      data: 'toBeDeleted6',
    }));
    dstPut(VersionedItem.create({
      id: 7,
      data: 'toBeStillDeleted7',
    }));
    let same3version;
    let deleted7version;
    dstDAO.sync().then(() => dstWait())
      .then(() => {
        dstRemove(VersionedItem.create({
          id: 4,
          data: 'same4deletedToUndeleted',
        }));
        dstRemove(VersionedItem.create({
          id: 5,
          data: 'unlike5deletedToUndeleted',
        }));
        dstPut(VersionedItem.create({
          id: 7,
          data: 'toBeStillDeleted7',
        }));
        return dstDAO.sync().then(() => dstWait());
      }).then(() => datastoreDAO.find(3))
      .then(same3 => same3version = same3.version_)
      .then(() => datastoreDAO.find(7))
      .then(deleted7 => {
        deleted7version = deleted7.version_;
        expect(deleted7.deleted_).toBe(true);
      }).then(() => updater.unversion(dstDAO, cacheDAO))
      .then(() => updater.importData(srcDAO, cacheDAO, dstDAO))
      .then(() => dstDAO.sync())
      .then(() => datastoreDAO.select())
      .then(arraySink => {
        const a = arraySink.array;
        expect(a[0].id).toBe(1);
        expect(a[0].data).toBe('src1');
        expect(a[0].deleted_).toBe(false);

        expect(a[1].id).toBe(2);
        expect(a[1].data).toBe('src2');
        expect(a[1].deleted_).toBe(false);

        expect(a[2].id).toBe(3);
        expect(a[2].data).toBe('same3');
        expect(a[2].deleted_).toBe(false);
        expect(a[2].version_).toBe(same3version);

        expect(a[3].id).toBe(4);
        expect(a[3].data).toBe('same4deletedToUndeleted');
        expect(a[3].deleted_).toBe(false);

        expect(a[4].id).toBe(5);
        expect(a[4].data).toBe('different5deletedToUndeleted');
        expect(a[4].deleted_).toBe(false);

        expect(a[5].id).toBe(6);
        expect(a[5].deleted_).toBe(true);

        expect(a[6].id).toBe(7);
        expect(a[6].deleted_).toBe(true);
        expect(a[2].version_).toBe(deleted7version);

        expect(a.length).toBe(7);
      });
  });
});
