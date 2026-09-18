#include "vec.h"

int main() {
    const char *tainted = source();
    Vec<const char *> values;
    values.push(tainted);
    return 0;
}
