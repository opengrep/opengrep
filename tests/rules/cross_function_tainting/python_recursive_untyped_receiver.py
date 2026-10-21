class Walker:
    def walk(self, node):
        # ruleid: python_recursive_untyped_receiver
        sink(node)
        self.next_walker.walk(source())

    def count(self, node):
        # ok: python_recursive_untyped_receiver
        sink("constant")
        self.next_walker.count(source())
