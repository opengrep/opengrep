//OK: a skipped element is not an expression
function f([, b]) {}

//MATCH:
function g([a, b]) {}
