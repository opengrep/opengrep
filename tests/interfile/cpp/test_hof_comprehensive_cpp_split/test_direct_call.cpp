void test_direct_call() {
    directCall<std::string>([](std::string x) {
        // ruleid: test-hof-taint
        sink(x);
    }, source());
}
