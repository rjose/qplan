var chartsModule = angular.module("charts", []);

var width = 500;
var height = 500;

function QuadrantDemoCtrl($scope, $element) {
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

        var maxEffort = d3.max(dataset, function(d) {return d[0]});
        var maxValue = d3.max(dataset, function(d) {return d[1]});

        var xScale = d3.scale.linear()
                .domain([0, maxEffort])
                .range([0, width]);
        var yScale = d3.scale.linear()
                .domain([0, maxValue])
                .range([0, height]);

        svg.selectAll("circle")
                .data(dataset)
                .enter()
                .append("circle")
                .attr("cx", function(d) {return xScale(d[0])})
                .attr("cy", function(d) {return yScale(d[1])})
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
