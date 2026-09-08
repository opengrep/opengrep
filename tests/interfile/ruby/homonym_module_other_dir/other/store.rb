# A file of the same name elsewhere in the project, never required by
# sub/app.rb: same class, same method, same arity.
class Store
  def save(data)
    # ok: homonym-module-other-dir
    sink(data)
  end
end
