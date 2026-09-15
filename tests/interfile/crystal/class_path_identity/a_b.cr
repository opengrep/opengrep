module A
  class B
    def self.leak(v)
      # ruleid: class-path-identity
      sink(v)
    end
  end
end
