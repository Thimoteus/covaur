// module Util

exports.logAnything = function (x) {
  return function () {
    console.log(x);
    return {};
  }
}

exports.spy = function (x) {
  console.log(x);
  return x
}

exports.showDate = function (d) {
  return d.toDateString();
}
