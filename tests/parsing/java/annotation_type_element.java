// javaext: annotation type (@interface) elements are abstract methods,
// optionally with a 'default <value>'.
public @interface MyAnno {
    String value();
    int count() default 0;
    String[] names() default {};
    Class<?> type() default Object.class;
}
