require_relative 'User'
require_relative 'main'
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
