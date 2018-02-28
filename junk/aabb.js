'use strict';

const assert = require('assert');

function fromRect(x, y, w, h) {
  return {top: y, right: x + w, bottom: y + h, left: x};
}

function intersectAABB(x, y) { 
  if (x.top > y.bottom) return null;
  else if (x.bottom < y.top) return null;
  else if (x.left > y.right) return null;
  else if (x.right < y.left) return null;
  else return {
    top: x.top >= y.top && x.top <= y.bottom ? x.top : y.top,
    right: x.right >= y.left && x.right <= y.right ? x.right : y.right,
    bottom: x.bottom >= y.top && x.bottom <= y.bottom ? x.bottom : y.bottom,
    left: x.left >= y.left && x.left <= y.right ? x.left : y.left,
  };
}

assert.deepEqual(
  intersectAABB(
    fromRect(0, 0, 1, 1),
    fromRect(1, 1, 1, 1)
  ),
  {top: 1, left: 1, right: 1, bottom: 1}
);

assert.deepEqual(
  intersectAABB(
    fromRect(0, 0, 1, 1),
    fromRect(1, 1, 2, 2)
  ),
  {top: 1, left: 1, right: 1, bottom: 1}
);

assert.deepEqual(
  intersectAABB(
    fromRect(0, 0, 1, 1),
    fromRect(2, 2, 2, 2)
  ),
  null
);

assert.deepEqual(
  intersectAABB(
    fromRect(0, 0, 10, 1),
    fromRect(5, 0, 1, 5)
  ),
  {top: 0, left: 5, right: 6, bottom: 1}
);

assert.deepEqual(
  intersectAABB(
    fromRect(0, 0, 1, 10),
    fromRect(0, 5, 5, 1)
  ),
  {top: 5, left: 0, right: 1, bottom: 6}
);
