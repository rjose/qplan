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

function WorkCtrl($scope, $http) {
    $scope.default_track = "All";
    $scope.tracks = [$scope.default_track];
    $scope.selected_track = $scope.default_track;
    $scope.triage = 1.5;
    $scope.track_stats = {};
    $scope.work_items = [];
    $scope.staff_by_skill = {};

    $scope.update = function() {
        $http.get('/app/web/qplan?triage=' + $scope.triage + "&track=" + $scope.selected_track).then(
            function(res) {
            console.dir(res);

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

    $scope.selectTrack = function(track) {
        angular.forEach($scope.tracks, function(t) {
            if (t == track) {
                $scope.selected_track = t;
                $scope.update();
            }
        });
    };



    // A helper function to convert a tags hash to a string
    $scope.tags_to_string = function(tags) {
        var keys = [];
        for (var key in tags) {
            keys.push(key)
        }
        keys.sort()

        var result = "";
        for (var i in keys) {
            var key = keys[i];
            if (tags[key]) {
                result = result + key + ": " + tags[key] + ", "
            }
        }
        // Get rid of trailing comma
        if (result.length >= 2) {
            result = result.slice(0, result.length-2);
        }
        return result;
    };

    // Update
    $scope.update();
}

