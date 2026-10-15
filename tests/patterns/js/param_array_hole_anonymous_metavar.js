//MATCH:
function f([, b]) {}

//MATCH:
function g([a, b]) {}

//OK: three elements
function h([a, , c]) {}
