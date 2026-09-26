func f() {
    // ERROR:
    a.run { [unowned self] in
        self.go()
    }
    b.run { [weak self] in
        self?.go()
    }
    c.run { [self] in
        self.go()
    }
    d.run {
        go()
    }
}
