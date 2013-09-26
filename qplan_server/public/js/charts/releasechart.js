if (typeof(charts.releasechart) != 'undefined') return;

//==============================================================================
// Static declarations
//

var yValue = null;
var band = null;


//==============================================================================
// Public API
//

charts.releasechart = {

   //---------------------------------------------------------------------------
   // Draws release chart in svg element.
   //
   draw: function(svg, scope) {
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
      charts.setChartHeight(svg, height);
      charts.setChartWidth(svg, width);

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
}

//==============================================================================
// Static functions
//

//------------------------------------------------------------------------------
// Computes y value for bands in releasechart
//
yValue = function(i) {
   // TODO: These should be specifiable from the public API
   var topMargin = 35;
   var itemSep = 30;
   return topMargin + i * itemSep
}



//------------------------------------------------------------------------------
// Adds bands for each item in selection
//
band = function(selection, timeScale) {
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


