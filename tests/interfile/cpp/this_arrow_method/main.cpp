#include "store.h"

void run(Store *store) {
    const char *tainted = source();
    store->handle(tainted);
}
