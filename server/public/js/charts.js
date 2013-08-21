//==============================================================================
// Local declarations
//
var chartsModule = angular.module("charts", []);


//------------------------------------------------------------------------------
// Static declarations
//
var drawQuadChart = null;
var drawReleaseChart = null;


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
         var data = JSON.parse(event.data);
         if (data.command == 'raw') {
            $scope.type = 'raw';
            $scope.title = data.title;
            $scope.chart = {};
            $scope.raw = data.data;
            $scope.aux_title = '';
         }
         else {
            console.log("Got " + data.command);
         }
         $scope.$apply();
      };

      //------------------------------------------------------------------------
      // Sets data for quadchart demo
      //
      $scope.demoQuadChart = function() {
         $scope.chart = {
            type: "quadchart",
            options: {
            },
            dataset:  [
               {name: 'Task 1', has_ext_prereq: true, effort: 5, value: 20},
               {name: 'Task 22', has_ext_prereq: true, effort: 450, value: 90},
               {name: 'Task 54', is_ext_prereq: true, effort: 250, value: 50},
               {name: 'Task 91', effort: 100, value: 30}
            ]
         };
      }

      //------------------------------------------------------------------------
      // Sets data for quadchart demo
      //
      $scope.demoReleaseChart = function() {
         $scope.chart = {
            type: "releasechart",
            options: {
            },
            dataset:  [
               {
                  group: 'Phone',
                  releaseBands: [["Aug 26, 2013", "Aug 30, 2013"],
                                 ["Sep 26, 2013", "Sep 30, 2013"]],
                  featureDates: [
                     {name: "Feature 1", expected: "Aug 15, 2013", target: "Aug 20, 2013"},
                     {name: "Feature 2", expected: "Aug 19, 2013", target: "Aug 30, 2013"}
                     ]
               },
               {
                  group: 'Tablet',
                  releaseBands: [["Aug 26, 2013", "Aug 30, 2013"],
                                 ["Sep 26, 2013", "Sep 30, 2013"]],
                  featureDates: [
                     {name: "Feature T1", expected: "Sep 15, 2013", target: "Sep 20, 2013"},
                     {name: "Feature T2", expected: "Sep 19, 2013", target: "Sep 30, 2013"}
                     ]
               }
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
