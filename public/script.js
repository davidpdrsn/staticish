(function() {
  var toArray = function(args) {
    return Array.prototype.slice.call(args);
  };

  var flip = function(f) {
    return function() {
      var flippedArgs = toArray(arguments);
      flippedArgs.reverse();
      return f.apply(this, flippedArgs);
    };
  };

  var App = {};
  App.fixPrismClasses = function() {
    $("code").each(flip(function(el) {
      var classes = $(el).attr("class").split(" ");
      var newClasses = classes.forEach(function(name) {
        $(el).addClass("language-" + name);
      });
    }));
  };

  $(function() {
    App.fixPrismClasses();
  });
})();
