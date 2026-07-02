require_relative 'IntermethodClass'
require_relative 'main'
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
