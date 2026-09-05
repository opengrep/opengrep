void customForEach(char** arr, int size, void (*callback)(char*)) {
    for (int i = 0; i < size; i++) {
        callback(arr[i]);
    }
}
