#include "hof.h"

void directCall(void (*callback)(char*)) {
    callback(source());
}
