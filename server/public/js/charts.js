//==============================================================================
// Local declarations
//
var chartsModule = angular.module("charts", []);


//------------------------------------------------------------------------------
// Static declarations
//
var drawQuadChart = null;
var drawReleaseChart = null;
var drawReleaseChart2 = null;


//==============================================================================
// Controllers
//
chartsModule.controller("LiveViewCtrl",
   ['$scope',
   function($scope) {
      $scope.type = "chart";
      $scope.title = "Chart Demo";
      $scope.aux_title = "";
      $scope.chart = {};
      $scope.raw = "";

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
         }
         else {
            console.log("Got " + data.command);
         }
         $scope.$apply();
      };

      $scope.demoReleaseChart = function() {
         $scope.chart = {
            type: 'releasechart',
            options: {},
            dataset: [
               {group: 'Phone',
               releaseDates: ["Aug 30, 2013", "Sep 30, 2013"],
               features: [
                  {name: "Feature1", expected: "Aug 15, 2013", target: "Aug 30, 2013"},
                  {name: "Feature2", expected: "Aug 25, 2013", target: "Aug 30, 2013"}
               ]},
               {group: 'Tablet',
               releaseDates: ["Oct 15, 2013"],
               features: [
                  {name: "Tab1", expected: "Sep 15, 2013", target: "Oct 15, 2013"}
               ]}
            ]
         };
      }

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
               drawReleaseChart2(svg, scope);
            }

         });
      }
   }
});


//==============================================================================
// Static functions
//


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
   var width = svg.attr("width");
   var height = svg.attr("height");
   var dataset = chart.dataset;

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
   // Add main line
   selection.append("line")
      .attr("x1", function(d) {return timeScale(new Date(d.expected))})
      .attr("x2", function(d) {return timeScale(new Date(d.target))})
      .attr("y1", function(d, i) {return 20 + i * 20})
      .attr("y2", function(d, i) {return 20 + i * 20})
      .style("stroke-width", 2)
      .style("stroke", "purple");

   // Add start circle
   selection.append("circle")
      .attr("cx", function(d) {return timeScale(new Date(d.expected))})
      .attr("cy", function(d, i) {return 20 + i * 20})
      .attr("r", 5)
      .style("fill", "purple");

   // Add end circle
   selection.append("circle")
      .attr("cx", function(d) {return timeScale(new Date(d.target))})
      .attr("cy", function(d, i) {return 20 + i * 20})
      .attr("r", 5)
      .style("fill", "purple");
}


drawReleaseChart2 = function(svg, scope) {
   var chart = scope.chart;
   if (!chart.type) return;

   var width = svg.attr("width");
   var height = svg.attr("height");
   var timeScale = d3.time.scale();
   var dates = [];
   var groupMargin = 25;
   var itemSep = 10;
   var groupSep = 30;
   var topMargin = 20;
   var hMargin = 20;
   var features = [];

   //
   // Gather data together:
   //
   //    - Collect dates to set up time scale
   //    - Collect features to render
   //
   for (var i=0; i < chart.dataset.length; i++) {
      var d = chart.dataset[i];
      console.log(d.group);

      // Collect release dates
      for (var j=0; j < d.releaseDates.length; j++) {
         var date = d.releaseDates[j];
         dates.push(new Date(date));
      }

      // Collect feature dates
      for (var j=0; j < d.features.length; j++) {
         var feature = d.features[j];
         dates.push(new Date(feature.expected));
         dates.push(new Date(feature.target));
         features.push(feature);
      }
   }
   timeScale.domain([d3.min(dates) , d3.max(dates)]);
   timeScale.range([hMargin, width - hMargin]);

   //
   // Draw feature bands
   //
   svg.selectAll("g.band")
      .data(features)
      .enter()
      .append("g").attr("class", "band")
      .call(band, timeScale);
}


//------------------------------------------------------------------------------
// Draws release chart in svg element.
//
drawReleaseChart = function(svg, scope) {
   var chart = scope.chart;
   if (!chart.type) return;

   var timeScale = d3.time.scale();

   var width = svg.attr("width");
   var height = svg.attr("height");
   var topMargin = 25;
   var leftMargin = 25;
   var itemMargin = 15;
   var itemHeight = 20;
   var groupMargin = 25;
   var dates = [];
   var curY = topMargin;
   var groupYValues = {};
   var releaseBands = [];

   //
   // Collect dates from each group so we can set up the timescale
   //
   // Also compute y values for each feature as well as for the release bands.
   chart.dataset.forEach(function(d) {
      var group = d.group;
      groupYValues[group] = {
         featureYValues: [],
         bandYValues: [curY]
      };

      d.featureDates.forEach(function(feature) {
         dates.push(new Date(feature.expected));
         dates.push(new Date(feature.target));
         groupYValues[group].featureYValues.push(curY);
         curY += (itemHeight + itemMargin);
      });
      groupYValues[group].bandYValues.push(curY);
      curY += groupMargin;

      d.releaseBands.forEach(function(band) {
         var bandStart = new Date(band[0]);
         var bandEnd = new Date(band[1]);
         dates.push(bandStart);
         dates.push(bandEnd);

         // Store release band data
         releaseBands.push({
            dates: [bandStart, bandEnd],
            yVals: groupYValues[group].bandYValues
         });
      });

   });
   timeScale.domain([d3.min(dates) , d3.max(dates)]);
   timeScale.range([0, width]);


   // Add release bands
   svg.selectAll("rect.band")
      .data(releaseBands)
      .enter()
      .append("rect")
      .attr("class", "band")
      .attr("x", function(d) {return timeScale(d.dates[0])})
      .attr("y", function(d) {return d.yVals[0]})
      .attr("width", function(d) {
         return timeScale(d.dates[1]) - timeScale(d.dates[0]);
      })
      .attr("height", function(d) {
         return d.yVals[1] - d.yVals[0];
      })
      .style("fill", "blue");

   // TODO: Add group label

   // TODO: Add frame
   // TODO: Add title
   // TODO: Add date axis ticks

}
