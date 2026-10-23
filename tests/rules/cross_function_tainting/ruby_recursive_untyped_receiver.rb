class Walker
  def walk(node)
    # ruleid: ruby_recursive_untyped_receiver
    sink(node.label)
    node.next_node.walk(source())
  end

  def visit(data)
    # ruleid: ruby_recursive_untyped_receiver
    sink(data)
    @children.each { |child| child.visit(source()) }
  end

  def count(node)
    # ok: ruby_recursive_untyped_receiver
    sink("constant")
    node.next_node.count(source())
  end
end
