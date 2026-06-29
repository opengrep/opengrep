// javaext: 9, an 'open' module declaration (opens all its packages for
// reflection), so it carries no explicit 'opens' directives.
open module com.example.lib {
    requires java.base;
    exports com.example.lib.api;
}
