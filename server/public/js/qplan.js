//==============================================================================
// Local declarations
//

var qplanModule = angular.module("qplan", []);


//==============================================================================
// Controllers
//
qplanModule.controller("QPlanCtrl",
      ['$scope', '$location', '$http',
      function($scope, $location, $http) {

      //========================================================================
      // Scope variables
      //

      $scope.default_track = "All";
      $scope.tracks = [$scope.default_track];
      $scope.track = $scope.default_track;
      $scope.triage = 1;
      $scope.track_stats = {};
      $scope.work_items = [];
      $scope.staff_by_skill = {};

      //========================================================================
      // Controller functions
      //

      //------------------------------------------------------------------------
      // Updates page given new track and triage.
      //
      $scope.update = function(track, triage) {
         $http.get('/app/web/qplan?triage=' + triage + "&track=" + track).then(
         function(res) {
            var numWeeks = res.data.num_weeks;
            $scope.start_date = res.data.start_date
            $scope.end_date = res.data.end_date
            $scope.tracks = res.data.tracks;
            $scope.skills = res.data.skills;
            $scope.track_stats = toNumPeople(res.data.track_stats, numWeeks)
            $scope.work_items = res.data.work_items;
            $scope.staff_by_skill = res.data.staff_by_skill;
            $scope.track = track;
            $scope.triage = triage;
            $scope.num_triage = countNumTriage($scope.triage, $scope.work_items);

            setChartData($scope.track_stats, $scope.skills);
         },
         function(res) {
            console.log("Ugh.");
            console.dir(res);
         });
      };

      //------------------------------------------------------------------------
      // Changes current track.
      //
      $scope.selectTrack = function(track) {
         angular.forEach($scope.tracks, function(t) {
               if (t == track) {
                  // TODO: Change the URL instead
                  //$scope.update(track, $scope.triage);
                  setPath(track, $scope.triage);
               }
         });
      };

      $scope.triageChanged = function() {
         setPath($scope.track, $scope.triage);
      };

      //========================================================================
      // Controller behavior
      //
      $scope.$watch(function() {return $location.path()},
         function(newpath) {
            renderPath(newpath);
         }
      );


      //========================================================================
      // Internal functions
      //
      function countNumTriage(triage, work_items) {
         var result = 0;
         for (var i=0; i < work_items.length; i++) {
            if (work_items[i].triage <= triage) {
               result++;
            }
         }
         return result;
      }

      function renderPath(path) {
         var fields = path.split("/");
         var track = fields[1];
         var triage = fields[2];
         if (!track) {
            track = "All";
         }
         if (!triage) {
            triage = "1";
         }
         $scope.update(track, triage);
      }

      setPath = function(track, triage) {
         $location.path(track + '/' + triage);
      }

      setChartData = function(trackStats, skills) {
         var totalDemand = 0;
         for (var i=0; i < trackStats.demand.length; i++) {
            totalDemand = totalDemand + trackStats.demand[i];
         }

         var shortage = [];
         for (var i=0; i < trackStats.net_avail.length; i++) {
            shortage.push({label: skills[i], value: -trackStats.net_avail[i]});
         }

         $scope.chart = {
            type: "shortagechart",
               dataset: {
                  demand: [{label: "Required", value: totalDemand}],
                  shortage: shortage
               }
         };
      }

      //------------------------------------------------------------------------
      // Converts track stats from manpower to num people.
      //
      function toNumPeople(trackStats, numWeeks) {
         if (numWeeks <= 0) {
            return trackStats;
         }

         var numSkills = trackStats.manpower.length;
         for (var i = 0; i < numSkills; i++) {
            trackStats.manpower[i] = trackStats.manpower[i]/numWeeks;
            trackStats.demand[i] = trackStats.demand[i]/numWeeks;
            trackStats.net_avail[i] = trackStats.net_avail[i]/numWeeks;
         }

         return trackStats;
      }
      }]
);


//==============================================================================
// Directives
//

qplanModule.directive("chart", function() {
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

            var labelSize = 15;
            charts.shortagechart.draw(svg, scope, width, height, labelSize);
         });
      }
   } });
