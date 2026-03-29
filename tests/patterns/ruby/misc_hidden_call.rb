#ERROR: match
foo(1,2,3)

# an identifier on its own as a statement is probably an hidden funcall
# which is why ruby_to_generic.ml convert that in a Call
#ERROR: match
foo

a = bar
# In Ruby, a bare identifier with no prior assignment is a method call.
#ERROR: match
b = foo
