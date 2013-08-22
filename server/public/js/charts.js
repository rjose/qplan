// Check for existing charts namespace
if (!typeof(charts) == 'undefined') return;

charts = {
   FONT_FAMILY: "Courier New, Courier, monospace",

   setChartWidth: function(svg, w) {
      svg.attr("width", w);
      $('#chart').width(w);
   },

   setChartHeight: function(svg, h) {
      svg.attr("height", h);
      $('#chart').height(h);
   }
}
