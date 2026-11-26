class T {}

// The T of the annotation is the class above, outside the scope of the type
// parameter of B.
// ok: annotation-refers-to-type-parameter
@Ann(T.class)
class B<T> {}
