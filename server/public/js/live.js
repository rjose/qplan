var Live = {};

Live.FULL_WIDTH = 960;
Live.FULL_HEIGHT = 768;
Live.FONT_FAMILY = "Courier New, Courier, monospace";

Live.add_svg = function(width, height) {
        var svg = d3.select('#output')
                .append("svg")
                .attr("width", width)
                .attr("height", height);

        return svg;
}

Live.draw_piechart = function(data, svg) {
        //
        // Parameters
        //
        var outerRadius = svg.attr("height") / 2.0 * 0.8;
        var innerRadius = outerRadius/2.0;
        var leftMargin = 15;
        var topMargin = 70;
        var labelSize = 20;
        var titleBaseline = 40;
        var titleSize = 36;
        var color = d3.scale.category10();

        //
        // Add pie wedges
        //
        var arc = d3.svg.arc().innerRadius(innerRadius)
                              .outerRadius(outerRadius);
        var arcs = svg.selectAll("g.arc")
                .data(d3.layout.pie()(data.values))
                .enter()
                .append("g")
                .attr("class", "arc")
                .attr("transform", "translate(" +
                                        (outerRadius + leftMargin) + "," +
                                        (outerRadius + topMargin) + ")");
        arcs.append("path")
                .attr("fill", function(d, i) {
                        return color(i);
                })
                .attr("d", arc);

        //
        // Add pie wedge labels
        //
        arcs.append("text")
            .attr("transform", function(d) {
                    return "translate(" + arc.centroid(d) + ")";
            })
            .attr("text-anchor", "middle")
            .attr("fill", "white")
            .attr("font-size", labelSize + "px")
            .attr("font-family", Live.FONT_FAMILY)
            .text(function(d, i) {
                    return data.labels[i];
            });

        //
        // Add chart title
        //
        svg.append("text")
           .attr("x", outerRadius + leftMargin)
           .attr("y", titleBaseline)
           .style("font-size", titleSize + "px")
           .attr("font-family", Live.FONT_FAMILY)
           .style("text-anchor", "middle")
           .text(data.title);
}


Live.render_data = function(data) {
        $('#output').empty();

        if (data.command == 'raw') {
                $('#output').append('<pre></pre>');
                $('#output > pre').text(data.data);
        }
        else if (data.command == 'piechart') {
                var svg = Live.add_svg(Live.FULL_WIDTH, Live.FULL_HEIGHT);
                Live.draw_piechart(data.data, svg);
        }
}
