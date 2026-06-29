public @interface MyAnno {
    // an annotation element is now a matchable abstract method
    // MATCH:
    String value();
    // an element with a 'default' value still matches as a method
    // MATCH:
    int count() default 0;
}
