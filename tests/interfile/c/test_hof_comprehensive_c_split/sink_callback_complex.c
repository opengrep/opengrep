void sink_callback_complex(char* node) {
    char* changes = node;
    // ruleid: test-hof-taint
    sink(changes);
}
