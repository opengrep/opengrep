void directCall(void (*callback)(char*)) {
    callback(source());
}
