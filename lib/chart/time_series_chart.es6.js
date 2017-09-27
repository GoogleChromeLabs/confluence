// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

let d3 = require('d3');

foam.CLASS({
  name: 'TimeSeriesChart',
  package: 'org.chromium.apis.web',
  documentation: `Helps to build multi time series charts on a given metric.`,
  properties: [
    {
      name: 'metric',
      documentation: `A metric is a JSON of form:
          {<browserName>: [Data Object, ...], ...}`,
      typeName: 'JSON',
      required: true,
      final: true,
    },
    {
      name: 'metricSeries',
      final: true,
      factory: function() {
        return Object.keys(this.metric).map((browserName) => {
          return {
            browserName,
            values: this.metric[browserName],
          };
        });
      },
    },
    {
      name: 'chartId',
      documentation: 'Id of the chart div in template file.',
      required: true,
      final: true,
    },
    {
      name: 'title',
      documentation: 'Title of this chart.',
      required: true,
      final: true,
    },
    {
      name: 'tooltipGen',
      documentation: 'A function of how tooltips are created',
      required: true,
      final: true,
    },
    {
      name: '$scope',
      documentation: '$scope from AngularJS',
      required: true,
      final: true,
    },
   ],
  methods: [
    {
      name: 'init',
      documentation: `Draw multi time series charts using d3 library.`,
      code: function() {
        // Return if this chart has insufficient data.
        if (this.metricSeries.length === 0) return;
        let width = 720;
        let height = 360;
        let margin = {top: 20, right: 50, bottom: 30, left: 50};
        // Create a sub div inside charts div.
        let div = d3.select(`#${this.chartId}`).append('div');
        // Create tootip div inside div.
        let tooltips = d3.select(`#${this.chartId}-tooltip`);
        // Create an svg element inside div.
        let svg = div.append('svg')
            .attr('width', width)
            .attr('height', height);
        // Append title to SVG.
        svg.append('text')
            .attr('x', (width / 2))
            .attr('y', margin.top)
            .attr('text-anchor', 'middle')
            .style('font-size', '20px')
            .style('text-decoration', 'underline')
            .text(this.title);
        // Set actual chart width and height.
        width = svg.attr('width') - margin.left - margin.right;
        height = svg.attr('height') - margin.top - margin.bottom;
        // Create a g element inside svg,
        // which will contain all paths and areas.
        let g = svg.append('g')
            .attr('transform', 'translate(' +
                margin.left + ',' + margin.top + ')');
        let rightAxisUpperBound = Math.ceil(d3.max(this.metricSeries,
            (apiNumArrary) => {
          return d3.max(apiNumArrary.values, (d) => d.value);
        }) / 20) * 20;
        let rightAxisLowerBound = 0;
        // Since the date for each release array is the same.
        let dates = this.metricSeries[0].values.map((d) => d.date);
        // X axis is release date.
        let x = d3.scaleTime()
            .rangeRound([0, width])
            .domain(d3.extent(dates));
        // Y axis is number of APIs.
        let y = d3.scaleLinear()
            .rangeRound([height, 0])
            .domain([rightAxisLowerBound, rightAxisUpperBound]);
        // Z domain is releases.
        // Use color schemes to work with d3.scaleOrdinal.
        let z = d3.scaleOrdinal(d3.schemeCategory10)
            .domain(this.metricSeries.map((b) => b.browserName));
        // Draw aixises.
        g.append('g')
            .attr('transform', 'translate(0,' + height + ')')
            .call(d3.axisBottom(x));
        g.append('g')
                .call(d3.axisLeft(y))
            .append('text')
                .attr('transform', 'rotate(-90)')
                .attr('fill', '#000')
                .attr('y', 6)
                .attr('dy', '0.71em')
                .attr('text-anchor', 'end')
                .text('#APIs');

        let line = d3.line()
            .x((d) => x(d.date))
            .y((d) => y(d.value));
        svg.selectAll('dot')
                .data(this.metricSeries.reduce((acc, curr) => {
                  return acc.concat(curr.values);
                }, []))
            .enter().append('circle')
                .attr('r', 5)
                .attr('cx', (d) => x(d.date) + margin.left)
                .attr('cy', (d) => y(d.value) + margin.top)
                .style('fill', (d) => z(d.browserName))
                .style('cursor', 'pointer')
                .on('click', function(d) {
                  d3.event.stopPropagation();
                  this.$scope.tooltip = this.tooltipGen(d);
                  tooltips.style('opacity', .9)
                      .style('left', (d3.event.pageX) + 'px')
                      .style('top', (d3.event.pageY) + 'px');
                  this.$scope.$apply();
                }.bind(this));
        // Click on other part of the chart, close the tooltip.
        svg.on('click', function() {
          this.$scope.tooltip = null;
          tooltips.style('opacity', 0);
          this.$scope.$apply();
        }.bind(this));
        let release = g.selectAll('.release')
                .data(this.metricSeries)
            .enter().append('g')
                .attr('class', 'release');
        // Draw lines for each brwoser.
        release.append('path')
            .attr('class', 'line')
            .attr('d', (d) => line(d.values))
            .style('stroke', (d) => z(d.browserName));
        // Add label for each line.
        release.append('text').datum((d) => {
          return {
            id: d.browserName,
            value: d.values[d.values.length - 1],
          };
        }).attr('transform', (d) =>`translate(${x(d.value.date)},
            ${y(d.value.value)})`)
            .attr('x', 3)
            .attr('dy', '0.35em')
            .style('font', '10px')
            .text((d) => d.id);
      },
    },
  ],
});
