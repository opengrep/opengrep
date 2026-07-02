#include <stdlib.h>
#include "handler.h"

const char *source(void) {
    return getenv("SECRET");
}

int main(void) {
    const char *tainted = source();
    handle(tainted);
    return 0;
}
