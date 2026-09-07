// An expression-bodied arrow returns its expression.
//ERROR:
var h = () => g(t);

// A block-bodied arrow returns nothing unless it says so; its last
// statement is not a return.
var f = () => {
  //OK:
  g(t);
};

var k = () => {
  //ERROR:
  return g(t);
};
