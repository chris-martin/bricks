(function() {
 
  var t = {
    data: function(x) {
      return $.map(x, function(courses, semester) {
        semester = t.semester(semester);
        return $.map(courses, function(course) {
          course = t.course(course);
          return $.extend({}, semester, course);
        });
      });
    },
    semester: function(x) {
      x = x.split(/\s+/);
      return { season: x[0], year: x[1] };
    },
    course: function(x) {
      return $.extend({}, t.courseNumber(x[0]),
        { hours: x[1], grade: x[2], name: x[3] });
    },
    courseNumber: function(x) {
      x = x.split(/\s+/);
      return { subject: x[0], course: x[1] };
    }
  };

  $(function() {

    $.ajax('gt-class.json', {
      success: function(data) {
        var rows = $.map(t.data(data), function(x) {
          return $('<tr/>').append([
            $('<td/>').text(x.subject),
            $('<td/>').text(x.course),
            $('<td/>').text(x.name)
          ])[0];
        });
        $('<table/>').appendTo('body').append(rows);
      }
    });

  });

})(jQuery);
