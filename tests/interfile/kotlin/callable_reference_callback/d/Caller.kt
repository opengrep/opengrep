package d

import b.Handler
import c.directCall

fun runB() {
    directCall(Handler::handle, source())
}
