val my_var = my_input("my_paramater") ?: return
// ruleid:taint-elvis
my_sink(my_var)

val my_var = my_input("my_paramater")
// ruleid:taint-elvis
val result = my_sink(my_var) ?: return

val my_var = my_input("my_paramater") ?: return
val my_sanitized_var = my_sanitizer(my_var)
// ok:taint-elvis
my_sink(my_sanitized_var)

val my_var = my_input("my_paramater") 
val my_sanitized_var = my_sanitizer(my_var) ?: return
// ok:taint-elvis
my_sink(my_sanitized_var)
