// @flow
type Point = {+x: number, -y: number};
function move(p: Point) {
  // MATCH:
  update(p.x);
  read(p.x);
}
