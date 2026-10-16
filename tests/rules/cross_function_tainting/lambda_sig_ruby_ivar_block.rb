# A block applied where it is used reads the receiver's fields there: one the
# method set itself, and one a helper method it calls memoises.
class Handler
  def update
    @item = source()
    respond do |r|
      # ruleid: lambda-sig-ruby-ivar-block
      r.go { sink(@item) }
    end
  end

  def destroy
    owner
    respond do |r|
      # ruleid: lambda-sig-ruby-ivar-block
      r.go { sink(owner) }
    end
  end

  def show
    label
    respond do |r|
      # ok: lambda-sig-ruby-ivar-block
      r.go { sink(label) }
    end
  end

  def owner
    @owner ||= source()
  end

  def label
    @label ||= "x"
  end
end
