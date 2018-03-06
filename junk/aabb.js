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

/**
 * https://www.gamedev.net/articles/programming/general-and-gameplay-programming/swept-aabb-collision-detection-and-response-r3084/
 */
function sweepAABB(v, a, b) {
  const xInvEntry = v.x > 0 ? b.left - a.right : b.right - a.left;
  const xInvExit = v.x > 0 ? b.right - a.left : b.left - a.right;
  const yInvEntry = v.y > 0 ? b.top - a.bottom : b.bottom - a.top;
  const yInvExit = v.y > 0 ? b.bottom - a.top : b.top - a.bottom;
  const xEntry = v.x === 0 ? -Infinity : xInvEntry / v.x;
  const xExit = v.x === 0 ? Infinity : xInvExit / v.x;
  const yEntry = v.y === 0 ? -Infinity : yInvEntry / v.y;
  const yExit = v.y === 0 ? Infinity : yInvExit / v.y;
  const entryTime = Math.max(xEntry, yEntry);
  const exitTime = Math.max(xExit, yExit);
  if (entryTime > exitTime || xEntry < 0 && yEntry < 0 || xEntry > 1 || yEntry > 1) {
    // normal = 0 0
    return null;
  } else {
    if (xEntry > yEntry) {
      if (xInvEntry < 0) {
        // normal = 1 0
      } else {
        // normal = -1 0
      }
    } else {
      if (yInvEntry < 0) {
        // normal = 0 1
      } else {
        // normal = 0 -1
      }
    }
    return {time: entryTime, normal: null};
  }
}

assert.deepEqual(null, sweepAABB({x: 0, y: 0}, fromRect(0, 0, 1, 1), fromRect(1, 0, 1, 1)));
assert.deepEqual({time: 0.25, normal: null}, sweepAABB({x: 4, y: 0}, fromRect(0, 0, 1, 1), fromRect(2, 0, 1, 1)));
assert.deepEqual({time: 0.25, normal: null}, sweepAABB({x: 0, y: 4}, fromRect(0, 0, 1, 1), fromRect(0, 2, 1, 1)));
assert.deepEqual({time: 0.50, normal: null}, sweepAABB({x: 2, y: 2}, fromRect(0, 0, 1, 1), fromRect(2, 2, 1, 1)));
assert.deepEqual({time: 0.50, normal: null}, sweepAABB({x: -2, y: -2}, fromRect(2, 2, 1, 1), fromRect(0, 0, 1, 1)));
