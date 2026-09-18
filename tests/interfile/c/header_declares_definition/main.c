#include "a/handler.h"

int main(void) {
    const char *tainted = source();
    handle(tainted);
    return 0;
}
