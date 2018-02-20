'use strict';

exports.hot = function() {
  if (module.hot) {
    module.hot.accept();
  }
}
