"abc"

// MATCH:     
"${i}abc"

// MATCH:     
"${i}${j}abc"
     
// MATCH:     
"xyz${i}qwe${j}abc"

"abc${i}"
     
"${i}${j}"
