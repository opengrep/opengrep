#include "a.h"

int main() {
    const char *tainted = source();
    handle(tainted);
    return 0;
}
