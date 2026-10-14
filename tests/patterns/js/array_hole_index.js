//MATCH:
[a] = arr;

//MATCH: a trailing comma is not an element
[a,] = arr;

//OK: two elements
[, b] = arr;
