#include "handler.h"

int main() {
    const char *tainted = source();
    handle(tainted, 1);
    return 0;
}
