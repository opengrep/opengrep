module A
  module B
    class Runner
      def run
        Audit.new.write(source())
      end
    end
  end
end
