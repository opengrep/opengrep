#include "pub.h"

int main(void) {
    const char *tainted = source();
    handle(tainted);
    handle_local(tainted);
    return 0;
}
