def top_helper
  source()
end

def top_argument
  # ruleid: ruby_bare_call_to_bound_method
  sink(top_helper)
end

module Uploads
  def mount
    source()
  end

  def mount_argument
    # ruleid: ruby_bare_call_to_bound_method
    sink(mount)
  end

  def mount_assigned
    m = mount
    # ruleid: ruby_bare_call_to_bound_method
    sink(m)
  end

  def shadowed
    mount = "safe"
    # ok: ruby_bare_call_to_bound_method
    sink(mount)
  end
end

module Visibility
  private def hidden
    source()
  end

  def hidden_argument
    # ruleid: ruby_bare_call_to_bound_method
    sink(hidden)
  end
end

module Singleton
  def self.helper
    source()
  end

  def self.helper_argument
    # ruleid: ruby_bare_call_to_bound_method
    sink(helper)
  end
end

module Conditional
  if true
    def maybe
      source()
    end
  end

  def maybe_argument
    # ruleid: ruby_bare_call_to_bound_method
    sink(maybe)
  end
end
