int main() {
    char* tainted_input = get_tainted_data();
    char* result = process_data(tainted_input);
    return 0;
}