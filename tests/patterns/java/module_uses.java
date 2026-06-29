// module descriptor (module-info.java); the pattern matches the whole
// declaration and binds the module name and the used service type.
// MATCH:
module com.example.app {
    uses com.example.app.spi.Plugin;
}
