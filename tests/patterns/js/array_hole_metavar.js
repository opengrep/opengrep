//OK: a skipped element is not an expression
[, b] = arr;

//MATCH:
[a, b] = arr;

//OK: three elements
[a, , c] = arr;
