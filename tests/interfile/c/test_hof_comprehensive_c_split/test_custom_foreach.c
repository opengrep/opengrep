void test_custom_foreach() {
    char* arr[] = {source()};
    customForEach(arr, 1, &sink_callback);
}
