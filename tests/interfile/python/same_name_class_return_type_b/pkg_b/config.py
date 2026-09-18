from pkg_b.safe import Safe


class Config:
    def load(self):
        return Safe()
