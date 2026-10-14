// a hole in an array literal is an element, a single trailing comma is not

//MATCH:
foo([1, 3]);

//MATCH:
foo([1, 3,]);

//OK:
foo([1, , 3]);

//OK:
foo([1, 3, ,]);
