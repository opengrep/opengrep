# A block in a method calling itself, whose signature is extracted more than
# once in a fixpoint, finds the flow through [key] once its signature is known.
class Finder
  def item
    memo do
      next nil unless item
      # ruleid: lambda-sig-ruby-reanalysis
      sink(key)
      # ok: lambda-sig-ruby-reanalysis
      sink(label)
    end
  end

  def key
    source()
  end

  def label
    "x"
  end
end
