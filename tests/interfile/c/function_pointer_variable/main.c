#include "handlers.h"

void (*cb)(const char *) = &handler;

void (*twice)(const char *);
void (*twice)(const char *) = &second;

int main(void) {
    const char *tainted = source();
    cb(tainted);
    twice(tainted);
    return 0;
}
