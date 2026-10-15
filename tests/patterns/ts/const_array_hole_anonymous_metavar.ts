//MATCH:
const [, d]: string[] = arr;

//MATCH:
const [c, d]: string[] = arr;

//OK: three elements
const [e, , f]: string[] = arr;
