class Finder
  def execute
    source()
  end

  def other_class_instance
    groups = OtherFinder.new(1).execute
    # ok: ruby_receiver_of_other_class
    sink(groups)
  end

  def local_of_other_class
    diffy = Diffy::Diff.new("a", "b")
    # ok: ruby_receiver_of_other_class
    sink(diffy.execute)
  end

  def validate(schema)
    # ok: ruby_receiver_of_other_class
    sink(schema.validate)
    source()
  end

  def explicit_self
    # ruleid: ruby_receiver_of_other_class
    sink(self.execute)
  end

  def implicit_self
    # ruleid: ruby_receiver_of_other_class
    sink(execute)
  end

  def same_class_instance
    # ruleid: ruby_receiver_of_other_class
    sink(Finder.new.execute)
  end
end
