var chartsModule = angular.module("charts", []);

var width = 500;
var height = 500;

function QuadrantDemoCtrl($scope, $element) {
        var svg = d3.select($element[0])
                .append("svg")
                .attr("width", width)
                .attr("height", height);

        var margin = 10;

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
