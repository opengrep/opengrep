
val taintedVar = my_source()
taintedVar.let {
    // ruleid:taint-scope-fn
    my_sink(it)
}

val taintedVar = my_source()
taintedVar.also {
    // ruleid:taint-scope-fn
    my_sink(it)
}

val taintedVar = my_source()
taintedVar.let {
    val sanitized = my_sanitizer(it)
    // ok:taint-scope-fn
    my_sink(sanitized)
}

val taintedVar = my_source()
taintedVar.also {
    val sanitized = my_sanitizer(it)
    // ok:taint-scope-fn
    my_sink(sanitized)
}

val taintedVar = my_source()
taintedVar.let { scope_var ->
    // ruleid:taint-scope-fn
    my_sink(scope_var)
}

val taintedVar = my_source()
taintedVar.also { scope_var ->
    // ruleid:taint-scope-fn
    my_sink(scope_var)
}

val taintedVar = my_source()
taintedVar.let { scope_var ->
    val sanitized = my_sanitizer(scope_var)
    // ok:taint-scope-fn
    my_sink(sanitized)
}

val taintedVar = my_source()
taintedVar.also { scope_var ->
    val sanitized = my_sanitizer(scope_var)
    // ok:taint-scope-fn
    my_sink(sanitized)
}

my_source().let { scope_var ->
    // ruleid:taint-scope-fn
    my_sink(scope_var)
}

my_source().also { scope_var ->
    // ruleid:taint-scope-fn
    my_sink(scope_var)
}

my_source().let { scope_var ->
    val sanitized = my_sanitizer(scope_var)
    // ruleid:taint-scope-fn
    my_sink(scope_var)
}

my_source().also { scope_var ->
    val sanitized = my_sanitizer(scope_var)
    // ruleid:taint-scope-fn
    my_sink(scope_var)
}