#include "conv.h"

int main() {
    const char *tainted = source();
    handle(1, tainted);
    drop(1, tainted);
    return 0;
}
