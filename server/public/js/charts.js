//==============================================================================
// Local declarations
//
var chartsModule = angular.module("charts", []);


//------------------------------------------------------------------------------
// Static declarations
//
var drawQuadChart = null;


//==============================================================================
// Controllers
//
chartsModule.controller("LiveViewCtrl",
   ['$scope',
   function($scope) {
      $scope.title = "Chart Demo";
      $scope.chart = {};

      // Sets data for chart
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
               drawQuadChart(svg, scope.chart);
            }

         });
      }
   }
});


//==============================================================================
// Static functions
//


//------------------------------------------------------------------------------
// Draws quadchart in svg element using data from "chart".
//
drawQuadChart = function(svg, chart) {
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
                   scope.name = d.name;
                   scope.$apply();

                   //svg.selectAll("*").remove();
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
