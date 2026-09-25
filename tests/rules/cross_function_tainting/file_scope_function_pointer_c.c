// A call through a function pointer at file scope reaches the function the
// pointer holds when the call runs: any function whose address it is given.
void first(char *x) {
    // ruleid: file_scope_function_pointer_c
    sink(x);
}

void second(char *x) {
    // ruleid: file_scope_function_pointer_c
    sink(x);
}

void unused(char *x) {
    // ok: file_scope_function_pointer_c
    sink(x);
}

void (*handler)(char *) = &first;
void (*other)(char *) = &unused;

void change(void) {
    handler = &second;
}

void use(void) {
    handler(source());
}
