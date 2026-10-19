const Shape = make();

interface Shape {
  x: number;
}

function draw(s: Shape) {
  return use(Shape);
}
