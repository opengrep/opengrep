void test_original_example() {
    char* history = getHistory("name", "owner");
    char* arr[] = {history};
    customForEach(arr, 1, &sink_callback_complex);
}
