// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

beforeAll(() => {
  global.createLocalRunner = (args, ctx) => {
    const pkg = org.chromium.apis.web;
    const finalArgs = Object.assign({
      numWorkers: 0,
      metricComputerService: pkg.MetricComputerService.create({
        // Directly inject properties that are required or for which the factory
        // should not be run.
        releasePredicate: ctx.releasePredicate,
        releaseDAO: ctx.releaseDAO,
        webInterfaceDAO: ctx.webInterfaceDAO,
        releaseWebInterfaceJunctionDAO: ctx.releaseWebInterfaceJunctionDAO,
        browserMetricsDAO: ctx.browserMetricsDAO,
        apiVelocityDAO: ctx.apiVelocityDAO,
      }, ctx),
    }, args);
    return pkg.MetricComputerRunner.create(finalArgs, ctx);
  };
});
