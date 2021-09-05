"use strict";

exports.windowPerformanceNow = function (window) {
  return function () {
    return window.performance.now();
  };
};
