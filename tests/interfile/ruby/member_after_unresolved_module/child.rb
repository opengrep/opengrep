require_relative 'base'

class Child < Base
  include Enumerable
end

class Audited
  include Helpers
  include Enumerable
end

class OwnMember < ShadowedBase
  include Enumerable

  def handle(data)
    data
  end
end
