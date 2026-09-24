def collide(name)
  @name = source()
  # ok: ivar_param_collision
  sink(name)
end

def same_method(x)
  @n = source()
  # ruleid: ivar_param_collision
  sink(@n)
end
