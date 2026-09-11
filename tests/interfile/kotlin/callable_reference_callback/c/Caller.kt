package c

import a.Handler

fun runA() {
    directCall(Handler::handle, source())
}
