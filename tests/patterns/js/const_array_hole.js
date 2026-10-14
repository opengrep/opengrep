//OK: a skipped element is not an expression
const [, d] = arr;

//MATCH:
const [c, d] = arr;

//OK: three elements
const [e, , f] = arr;
