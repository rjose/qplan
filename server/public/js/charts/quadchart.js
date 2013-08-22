if (typeof(charts.quadchart) != 'undefined') return;

//==============================================================================
// Public API
//

charts.quadchart = {
   //---------------------------------------------------------------------------
   // Draws quadchart in svg element.
   //
   draw : function(svg, scope) {
      var chart = scope.chart;
   
      if (!chart.type) return;
   
      var margin = 20;
      var radius = 6;
      var hasExtPrereqColor = "orange";
      var isExtPrereqColor = "#d62728";
      var width = 500;
      var height = 500;
      var dataset = chart.dataset;
   
      charts.setChartWidth(svg, width);
      charts.setChartHeight(svg, height);
   
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
}
