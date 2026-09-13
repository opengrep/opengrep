#include "store.h"

int main() {
    const char *tainted = source();
    Store store;
    store.send(tainted);
    return 0;
}
