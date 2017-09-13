// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('angular');

let d3 = require('d3');

function apiVelocityChartController($scope, $timeout) {
  let createNewDiffView = function(minuend, subtrahend) {
    // Create a view that contains set of APIs that is
    // minuend set minus subtrahend.
    this.newDiffView({minuend, subtrahend});
  }.bind(this);

  this.$postLink = function() {
    // AngularJS does not seem to have reliable post-render callbacks.
    // Use $timeout as a hack.
    $timeout(function() {
      let width = 720;
      let height = 360;
      let margin = {top: 20, right: 50, bottom: 30, left: 50};
      // Create a sub div inside charts div.
      let div = d3.select(`#${this.chartId}`).append('div');
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
      let g = svg.append('g').attr('transform',
          'translate(' + margin.left + ',' + margin.top + ')');
      let rightAxisUpperBound = Math.ceil(d3.max(
          this.apiVelocity, (d) => d.totalApis + d.removedApis) / 500) * 500;
      let rightAxisLowerBound = Math.floor(d3.min(
          this.apiVelocity, (d) => d.totalApis - d.newApis) / 500) * 500;
      // X axis is release date.
      let x = d3.scaleTime()
          .rangeRound([0, width])
          .domain(d3.extent(this.apiVelocity, (d) => d.releaseDate));
      // Y axis is number of APIs.
      let y = d3.scaleLinear()
          .rangeRound([height, 0])
          .domain([rightAxisLowerBound, rightAxisUpperBound]);
      // Append tooltip div inside chart div. (cannot append div inside svg)
      let tooltips = d3.select(`#${this.chartId}-tooltip`);
      // Total number of APIs' area.
      let areaTotal = d3.area()
          .x((d) => x(d.releaseDate))
          .y1((d) => y(d.totalApis))
          .y0(y(rightAxisLowerBound));
      // Number of removed APIs.
      let areaRemovedAPI = d3.area()
          .x((d) => x(d.releaseDate))
          .y1((d) => y(d.totalApis + d.removedApis))
          .y0((d) => y(d.totalApis));
      // Number of new APIs.
      let areaNewAPI = d3.area()
          .x((d) => x(d.releaseDate))
          .y1((d) => y(d.totalApis))
          .y0((d) => y(d.totalApis - d.newApis));
      // Create dot where tooltips are displayed.
      svg.selectAll('dot')
              .data(this.apiVelocity)
          .enter().append('circle')
              .attr('r', 5)
              .attr('cx', (d) => x(d.releaseDate) + margin.left)
              .attr('cy', (d) => y(d.totalApis) + margin.top)
              .style('cursor', 'pointer')
              .on('click', function(d) {
                d3.event.stopPropagation();
                $scope.tooltip = {
                  browserName: d.browserName,
                  properties: [
                    {
                      name: 'Version',
                      value: d.browserVersion,
                    },
                    {
                      name: 'Total APIs',
                      value: d.totalApis,
                    },
                    {
                      name: 'New APIs',
                      value: d.newApis,
                      action: d.prevRelease ? function() {
                        createNewDiffView(d.currRelease, d.prevRelease);
                      } : undefined,
                    },
                    {
                      name: 'Removed APIs',
                      value: d.removedApis,
                      action: d.prevRelease ? function() {
                        createNewDiffView(d.prevRelease, d.currRelease);
                      } : undefined,
                    },
                  ],
                };
                tooltips.style('opacity', .9)
                    .style('left', (d3.event.pageX) + 'px')
                    .style('top', (d3.event.pageY) + 'px');
                $scope.$apply();
              });
      // Click on other part of the chart, close the tooltip.
      svg.on('click', function() {
        $scope.tooltip = null;
        tooltips.style('opacity', 0);
        $scope.$apply();
      });
      g.append('path')
          .datum(this.apiVelocity)
          .attr('fill', 'steelblue')
          .style('opacity', .2)
          .attr('d', areaTotal);
      g.append('path')
          .datum(this.apiVelocity)
          .attr('fill', 'green')
          .style('opacity', .2)
          .attr('d', areaNewAPI);
      g.append('path')
          .datum(this.apiVelocity)
          .attr('fill', 'red')
          .style('opacity', .2)
          .attr('d', areaRemovedAPI);
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
    }.bind(this));
  };
}

angular.module('confluence').component('apiVelocityChart', {
  template: require('../../static/component/chart.html'),
  controller: ['$scope', '$timeout', apiVelocityChartController],
  bindings: {
    apiVelocity: '<',
    title: '<',
    chartId: '<',
    newDiffView: '&',
  },
});
