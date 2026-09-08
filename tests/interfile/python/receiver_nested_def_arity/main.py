class Runner:
    def run(self, data):
        def helper(x):
            # ruleid: receiver-nested-def-arity
            sink(x)
        helper(data)

def go():
    Runner().run(source())
