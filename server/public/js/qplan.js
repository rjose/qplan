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
      $scope.selected_track = $scope.default_track;
      $scope.triage = 1.5;
      $scope.track_stats = {};
      $scope.work_items = [];
      $scope.staff_by_skill = {};

      //========================================================================
      // Controller functions
      //

      //------------------------------------------------------------------------
      // Updates page given new track and triage.
      //
      $scope.update = function() {
         $http.get('/app/web/qplan?triage=' + $scope.triage + "&track=" + $scope.selected_track).then(
         function(res) {
            var numWeeks = 13; // TODO: Get this from the server
            $scope.tracks = res.data.tracks;
            $scope.skills = res.data.skills;
            $scope.track_stats = toNumPeople(res.data.track_stats, numWeeks)
            $scope.work_items = res.data.work_items;
            $scope.staff_by_skill = res.data.staff_by_skill;
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
                  $scope.selected_track = t;
                  $scope.update();
               }
         });
      };



      //========================================================================
      // Internal functions
      //

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

      // Trigger update when starting
      $scope.update();

      //      //========================================================================
      //      // Static declarations
      //      //
      //      var renderPath;
      //      var setSelectedPath;
      //
      //      //------------------------------------------------------------------------
      //      // Updates "selected" scope variable when a chart is selected.
      //      //
      //      //    Typically, this will be called when someone clicks on one of the
      //      //    charts in the view.
      //      //
      //      $scope.selectChart = function(index) {
      //         if ($scope.selected == index) {
      //            setSelectedPath(null);
      //            $scope.selected = null;
      //         }
      //         else {
      //            setSelectedPath(index);
      //            $scope.selected = index;
      //         }
      //
      //      };
      //
      //
      //      //------------------------------------------------------------------------
      //      // Renders page whenever URL changes.
      //      //
      //      $scope.$watch(function() {return $location.path()},
      //         function(newpath) {
      //            renderPath(newpath.substring(1));
      //         }
      //      );
      //
      //
      //      //========================================================================
      //      // Internal functions
      //      //
      //
      //      //------------------------------------------------------------------------
      //      // Renders the state of the page given the path.
      //      //
      //      //    This is the funnel point for all changes to the page. If a user
      //      //    selects a chart, the render eventually comes through here.
      //      //
      //      //    We know when a page has been reloaded or visited directly because
      //      //    $scope.charts will not yet have been set. For this case, there
      //      //    should be no animation.
      //      //
      //      //    For the case where the user is interacting with the page, the charts
      //      //    will animate into place.
      //      //
      //      renderPath = function(path) {
      //         var matcher = /chart(\d+)/;
      //         var match = matcher.exec(path);
      //         if (match) {
      //            $scope.selected = parseInt(match[1]);
      //         }
      //         else {
      //            $scope.selected = null;
      //         }
      //
      //         // If need to load data, do so. This should cause a render without
      //         // animation.
      //         if (!$scope.charts) {
      //            // TODO: Make an ajax call to get them
      //            setTimeout(function() {
      //               $scope.charts = sample_charts;
      //               $scope.$apply();
      //            }, 0);
      //         }
      //      }
      //
      //
      //      //------------------------------------------------------------------------
      //      // Sets the hash fragment part of the URL
      //      //
      //      setSelectedPath = function(index) {
      //         if (index == null) {
      //            $location.path('main');
      //         }
      //         else {
      //            $location.path('chart' + index);
      //         }
      //      }

      }]
);

