exports.foreachWithIndex = function (xs) {
  return function (f) {
    return function () {
      for (var i = 0, l = xs.length; i < l; i++) {
        f(i)(xs[i])();
      }
    };
  };
};
