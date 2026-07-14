from mid import Mid


class Child(Mid):
    def run(self):
        # self.opt is an inherited @staticmethod (Base.opt, 2 hops up the MRO).
        self.opt(source())
        # self.helper is an inherited instance method (Base.helper).
        self.helper(source())
