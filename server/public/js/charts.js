//==============================================================================
// Local declarations
//
var chartsModule = angular.module("charts", []);


//------------------------------------------------------------------------------
// Static declarations
//
var drawQuadChart = null;
var drawReleaseChart = null;
var yValue = null;
var setChartWidth = null;
var setChartHeight = null;


//==============================================================================
// Controllers
//
chartsModule.controller("LiveViewCtrl",
   ['$scope',
   function($scope) {
      $scope.type = "";
      $scope.title = "";
      $scope.aux_title = "";
      $scope.chart = {};
      $scope.raw = "";
      $scope.fields = [];

      //
      // Set up websocket
      //
      var websocket = new WebSocket("ws://" + document.location.hostname + ":8888");
      websocket.onopen = function(event) {
         console.log("Open!");
      };
      websocket.onmessage = function(event) {
         var message = JSON.parse(event.data);
         if (message.command == 'raw') {
            $scope.type = 'raw';
            $scope.title = message.title;
            $scope.raw = message.data;
            $scope.chart = {};
            $scope.aux_title = '';
         }
         else if (message.command == 'chart') {
            console.log(message);
            $scope.type = 'chart';
            $scope.title = message.title;
            $scope.chart = message.data;
            $scope.raw = '';
            $scope.aux_title = '';
            $scope.fields = [];
         }
         else {
            console.log("Got " + data.command);
         }
         $scope.$apply();
      };

      // End function
   }]
);



//==============================================================================
// Directives
//
chartsModule.directive("chart", function() {
   return {
      link: function(scope, element, attrs) {
         var el = element[0];
         var width = el.offsetWidth;
         var height = el.offsetHeight;

         scope.$watch('chart', function() {
            if (!scope.chart) return;

            // Clear out contents before creating new chart
            $(el).empty();

            var svg = d3.select(element[0])
               .append("svg")
               .attr("width", width)
               .attr("height", height);

            if (scope.chart.type == 'quadchart') {
               drawQuadChart(svg, scope);
            }
            else if (scope.chart.type == 'releasechart') {
               drawReleaseChart(svg, scope);
            }

         });
      }
   } });


//==============================================================================
// Static functions
//

setChartWidth = function(svg, w) {
   svg.attr("width", w);
   $('#chart').width(w);
}

setChartHeight = function(svg, h) {
   svg.attr("height", h);
   $('#chart').height(h);
}

//------------------------------------------------------------------------------
// Computes y value for bands in releasechart
//
yValue = function(i) {
   var topMargin = 35;
   var itemSep = 30;
   return topMargin + i * itemSep
}

//------------------------------------------------------------------------------
// Draws quadchart in svg element.
//
drawQuadChart = function(svg, scope) {
   var chart = scope.chart;

   if (!chart.type) return;

   var margin = 20;
   var radius = 6;
   var hasExtPrereqColor = "orange";
   var isExtPrereqColor = "#d62728";
   var width = 500;
   var height = 500;
   var dataset = chart.dataset;

   setChartWidth(svg, width);
   setChartHeight(svg, height);

   var maxEffort = d3.max(dataset, function(d) {return d.effort});
   var maxValue = d3.max(dataset, function(d) {return d.value});

   var xScale = d3.scale.linear()
           .domain([0, maxEffort])
           .range([margin, width - margin]);
   var yScale = d3.scale.linear()
           .domain([0, maxValue])
           .range([margin, height - margin]);

   // Add quadrants
   var vertLine = svg.append("line")
           .attr("x1", height/2)
           .attr("y1", 0)
           .attr("x2", height/2)
           .attr("y2", width)
           .style("stroke-width", 2)
           .style("stroke", "grey");

   var horizLine = svg.append("line")
           .attr("x1", 0)
           .attr("y1", height/2)
           .attr("x2", width)
           .attr("y2", height/2)
           .style("stroke-width", 2)
           .style("stroke", "grey");

   var frame = svg.append("rect")
           .attr("x", 0)
           .attr("y", 0)
           .attr("width", width)
           .attr("height", height)
           .style("fill", "none")
           .style("stroke-width", 3)
           .style("stroke", "black");

   // Add circles
   svg.selectAll("circle")
           .data(dataset)
           .enter()
           .append("circle")
           .on("click", function(d) {
               // TODO: Figure out the best place to do this
               scope.aux_title = d.name;
               var fields = [
                {key: 'Effort', value: d.effort},
                {key: 'Value', value: d.value},
               ];
               if (d.has_ext_prereq) {
                  fields.push({key:'Dependent on other teams', value:'Yes'});
               }
               if (d.is_ext_prereq) {
                  fields.push({key:'Key for other teams', value:'Yes'});
               }
               scope.fields = fields;
               scope.$apply();
           })
           .attr("cx", function(d) {return xScale(d.effort)})
           .attr("cy", function(d) {return yScale(d.value)})
           .attr("r", radius)
           .style("fill", function(d) {
                   if (d.is_ext_prereq) {
                           return isExtPrereqColor;
                   }
                   else {
                           return "black";
                   }
           })
           .style("stroke-width", 3)
           .style("stroke", function(d) {
                   if (d.has_ext_prereq) {
                           return hasExtPrereqColor;
                   }
                   else {
                           return "none"
                   }
           });
}


function band(selection, timeScale) {
   var radius = 5;

   // Add main line
   selection.append("line")
      .attr("x1", function(d) {return timeScale(new Date(d.expected))})
      .attr("x2", function(d) {return timeScale(new Date(d.target))})
      .attr("y1", function(d, i) {return yValue(i)})
      .attr("y2", function(d, i) {return yValue(i)})
      .style("stroke-width", 2)
      .style("stroke", "purple");

   // Add start circle
   selection.append("circle")
      .attr("cx", function(d) {return timeScale(new Date(d.expected))})
      .attr("cy", function(d, i) {return yValue(i)})
      .attr("r", 5)
      .style("fill", "purple");

   // Add end circle
   selection.append("circle")
      .attr("cx", function(d) {return timeScale(new Date(d.target))})
      .attr("cy", function(d, i) {return yValue(i)})
      .attr("r", radius)
      .style("fill", "purple");

   // Add label
   var margin = 8;
   selection.append("text")
      .attr("x", function(d) {return timeScale(new Date(d.expected))})
      .attr("y", function(d, i) {return yValue(i) - margin})
      .attr("font-size", "11px")
      .text(function(d) {return d.name});
}


//------------------------------------------------------------------------------
// Draws release chart in svg element.
//
drawReleaseChart = function(svg, scope) {
   var chart = scope.chart;
   if (!chart.type) return;

   var features = chart.dataset.features;
   var releaseDates = [];
   var timeScale = d3.time.scale();
   var dates = [];
   var topMargin = 20;
   var hMargin = 15;
   var curY;
   var height;
   var width = 960;
   var axisHeight = 30;

   // Compute height based on number of features
   height = yValue(features.length) + axisHeight;
   setChartHeight(svg, height);
   setChartWidth(svg, width);

   // Pull data:
   //
   //    - Collect dates to set up time scale
   //    - Collect features to render
   //
   curY = topMargin;

   // Collect release dates
   for (var j=0; j < chart.dataset.releaseDates.length; j++) {
      var date = new Date(chart.dataset.releaseDates[j]);
      releaseDates.push(date);
      dates.push(date);
   }

   // Collect feature dates
   for (var j=0; j < features.length; j++) {
      var feature = features[j];
      dates.push(new Date(feature.expected));
      dates.push(new Date(feature.target));
   }
   timeScale.domain([d3.min(dates) , d3.max(dates)]);
   timeScale.range([hMargin, width - 2*hMargin]);



   // Draw release gridlilnes
   svg.selectAll("line.release-gridline")
      .data(releaseDates)
      .enter()
      .append("line").attr("class", "release-gridline")
      .attr("x1", function(d) {return timeScale(d)})
      .attr("x2", function(d) {return timeScale(d)})
      .attr("y1", 0)
      .attr("y2", height - axisHeight)
      .style("stroke-width", 1)
      .style("stroke", "gray");

   // Draw feature bands
   svg.selectAll("g.band")
      .data(features)
      .enter()
      .append("g").attr("class", "band")
      .call(band, timeScale);

   // Draw x axis
   var xAxis = d3.svg.axis()
      .scale(timeScale)
      .tickValues(releaseDates)
      .tickFormat(d3.time.format("%b %e"));

   svg.append("g")
      .attr("class", "x axis")
      .attr("transform", "translate(0," + (height - axisHeight) + ")")
      .call(xAxis);

   // Draw frame around chart
   svg.append("rect")
      .attr("x", 0)
      .attr("y", 0)
      .attr("width", width)
      .attr("height", height)
      .style("fill", "none")
      .style("stroke-width", 3)
      .style("stroke", "black");
}
