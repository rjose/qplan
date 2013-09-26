//==============================================================================
// Local declarations
//
var chartsModule = angular.module("live", []);

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
            $scope.fields = [];
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


//------------------------------------------------------------------------------
// Adds ability to render charts to elements.
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
               charts.quadchart.draw(svg, scope);
            }
            else if (scope.chart.type == 'releasechart') {
               charts.releasechart.draw(svg, scope);
            }
            else if (scope.chart.type == 'piechart') {
               charts.piechart.draw(svg, scope);
            }
            else if (scope.chart.type == 'shortagechart') {
               charts.shortagechart.draw(svg, scope);
            }

         });
      }
   } });
