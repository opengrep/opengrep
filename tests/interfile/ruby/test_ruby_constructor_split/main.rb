require_relative 'User'
require_relative 'IntermethodClass'
def main
  tainted_input = taint
  user = User.new(tainted_input)
  result = user.get_profile()

  # Test intermethod taint flow
  intermethod_obj = IntermethodClass.new()
  intermethod_result = intermethod_obj.sink_method()

  # Test chained method call: ClassName.new(tainted).method()
  # ruleid: ruby_constructor_sqli
  chained_result = "SELECT * FROM users WHERE name = #{User.new(taint).get_profile()}"

  return result
end
