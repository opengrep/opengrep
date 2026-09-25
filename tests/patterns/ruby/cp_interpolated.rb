age = "42"
query = "SELECT name FROM users WHERE age=#{age}"
# ERROR: match
foo(query)
# ERROR: match
foo("DELETE FROM table WHERE age=#{age}")

def bar
  age = "42"
  q = "SELECT name FROM users WHERE age=#{age}"
  # ERROR: match
  foo(q)
end

def baz
  # a method body does not see the file's local variables
  q = "SELECT name FROM users WHERE age=#{age}"
  foo(q)
end
