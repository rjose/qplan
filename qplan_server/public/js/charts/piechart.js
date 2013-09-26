if (typeof(charts.piechart) != 'undefined') return;


charts.piechart = {

   //------------------------------------------------------------------------------
   // Draws pie chart in svg element.
   //
   draw: function(svg, scope) {
      var chart = scope.chart;

      if (!chart.type) return;
      var height = 700;
      var width = 700;

      charts.setChartHeight(svg, height);
      charts.setChartWidth(svg, width);

      // Parameters
      //
      var outerRadius = height / 2.0 * 0.8;
      var innerRadius = outerRadius/2.0;
      var leftMargin = 15;
      var topMargin = 20;
      var labelSize = 20;
      var color = d3.scale.category10();
      var dataset = chart.dataset;

      var arc = d3.svg.arc().innerRadius(innerRadius)
         .outerRadius(outerRadius);

      var pie = d3.layout.pie()
         .value(function(d) {return d.value});

      var arcs = svg.selectAll("g.arc")
         .data(pie(chart.dataset))
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

      // Add pie wedge labels
      //
      arcs.append("text")
         .attr("transform", function(d) {
            return "translate(" + arc.centroid(d) + ")";
         })
      .attr("text-anchor", "middle")
         .attr("fill", "white")
         .attr("font-size", labelSize + "px")
         .attr("font-family", charts.FONT_FAMILY)
         .text(function(d) {
            return d.data.label;
         });
   }
}
