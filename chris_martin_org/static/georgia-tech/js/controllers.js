'use strict';

var gtHistoryApp = angular.module('gtHistoryApp', []);

gtHistoryApp.controller('CourseListCtrl', function($scope, $http) {

  var seasonMonth = {
    "Spring": 0,
    "Summer": 5,
    "Fall": 7
  };

  $http.get('courses.json').success(function (data) {

    $scope.courses = data.map(function (array) {
      return {
        year: array[0],
        season: array[1],
        number: array[2],
        hours: array[3],
        grade: array[4],
        name: array[5]
      };
    });

    $scope.semesters = _.map(
      _.groupBy(
        $scope.courses,
        function (course) {
          return JSON.stringify({
            year: course.year,
            season: course.season
          });
        }
      ),
      function (courses, semester) {
        semester = JSON.parse(semester);
        return {
          year: semester.year,
          season: semester.season,
          name: semester.season + " " + semester.year,
          time: new Date(semester.year, seasonMonth[semester.season]),
          courses: courses
        };
      }
    );
  });
});
