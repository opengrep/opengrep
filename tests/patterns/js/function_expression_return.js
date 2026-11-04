// A function expression returns only what a return gives; its last
// statement is not a return.
var f = function () {
  //OK:
  g(t);
};

var k = function () {
  //ERROR:
  return g(t);
};
