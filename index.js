$(function() {
  $.ajax('data/whatever.json', {
    success: function(data) {
      $.each(data.stuff, function(i, thing) {
        $('<div/>').text(thing).appendTo('body');
      });
    }
  });
});
