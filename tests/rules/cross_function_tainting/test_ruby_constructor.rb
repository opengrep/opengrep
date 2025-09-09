class User
  def initialize(user_name)
    @name = user_name
  end

  def get_profile
    # ruleid: ruby_constructor_sqli
    query = "SELECT * FROM users WHERE name = #{@name}"
    return query
  end
end

class IntermethodClass
  def taint_method
    return taint
  end

  def sink_method
    # ruleid: ruby_constructor_sqli
    query = "SELECT * FROM users WHERE name = #{self.taint_method()}"
    return query
  end
end

def main
  tainted_input = taint
  user = User.new(tainted_input)
  result = user.get_profile()

  # Test intermethod taint flow
  intermethod_obj = IntermethodClass.new()
  intermethod_result = intermethod_obj.sink_method()

  return result
end
