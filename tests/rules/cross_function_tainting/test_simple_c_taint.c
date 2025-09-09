char* get_tainted_data() {
    return source();
}

char* process_data(char* data) {
    // ruleid: simple_c_taint
    return sink(data);
}

int main() {
    char* tainted_input = get_tainted_data();
    char* result = process_data(tainted_input);
    return 0;
}