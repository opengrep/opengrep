void foo() {
    //ERROR: match
    a = 5;
    #ifdef DEBUG
    if (x == 2) {
      //ERROR: match
      b = 5;
    } else
    #else
    {
      //ERROR: match
      c = 5;
    }
    #endif
    //ERROR: match
    d = 5;
}

void goo() {
    //ERROR: match
    a = 5;
    #ifdef DEBUG
    //ERROR: match
    b = 5;
    #ifdef DEBUG
    //ERROR: match
    b = 5;
    #else
    //ERROR: match
    c = 5;
    #endif
    //ERROR: match
    x = 5;
    #else
    //ERROR: match
    c = 5;
    #ifdef DEBUG
    //ERROR: match
    b = 5;
    #else
    //ERROR: match
    c = 5;
    #endif
    //ERROR: match
    d = 5;
    #endif
    //ERROR: match
    d = 5;
}
