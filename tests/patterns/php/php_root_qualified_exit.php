<?php

// '\exit(...)' and '\die(...)' are exit, which no namespace can shadow

//MATCH:
exit(1);

//MATCH:
\exit(2);

//MATCH:
\die(3);

//OK:
print(4);
