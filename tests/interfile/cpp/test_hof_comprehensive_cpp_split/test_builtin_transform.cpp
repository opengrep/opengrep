void test_builtin_transform() {
    std::vector<std::string> arr = {source()};
    std::vector<std::string> result;
    std::transform(arr.begin(), arr.end(), std::back_inserter(result),
        [](std::string x) {
            // ruleid: test-hof-taint
            sink(x);
            return x;
        });
}
