// javaext: 9, module declaration (module-info.java).
// Exercises every module directive kind in the java_to_generic translation.
module com.example.app {
    requires java.base;
    requires transitive java.sql;
    requires static java.compiler;

    exports com.example.app.api;
    exports com.example.app.internal to com.example.client, com.example.test;

    opens com.example.app.model;
    opens com.example.app.impl to com.example.framework;

    uses com.example.app.spi.Plugin;

    provides com.example.app.spi.Plugin
        with com.example.app.impl.DefaultPlugin, com.example.app.impl.FastPlugin;
}
