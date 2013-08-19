var chartsModule = angular.module("charts", []);

var width = 500;
var height = 500;

function QuadrantDemoCtrl($scope, $element) {
        console.log("Howdy");
        console.log($element[0]);

        var svg = d3.select($element[0])
                .append("svg")
                .attr("width", width)
                .attr("height", height);

        // This dataset will have been sent by the client.
        var dataset = [
                [5, 20],
                [450, 90],
                [250, 50],
                [100, 30]
        ];

        svg.selectAll("circle")
                .data(dataset)
                .enter()
                .append("circle")
                .attr("cx", function(d) {return d[0];})
                .attr("cy", function(d) {return d[1];})
                .attr("r", 5);
}

chartsModule.directive("quadrantdemo", function() {
        return {
                restrict: "A",
                controller: QuadrantDemoCtrl
        };
});

chartsModule.filter('greet', function() {
        return function(name) {
                return "Hello, " + name + "!";
        }
});
