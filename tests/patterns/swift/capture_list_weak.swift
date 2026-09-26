func g() {
    a.run { [x] in
        use(x)
    }
    // ERROR:
    b.run { [weak x] in
        use(x)
    }
    c.run { [unowned x] in
        use(x)
    }
    // ERROR:
    d.run { [weak self] in
        self?.go()
    }
    e.run {
        use(x)
    }
}
