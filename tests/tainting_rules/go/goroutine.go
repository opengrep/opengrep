func foo() {
    src := source()

    // Control test: normal lambda call
    // ruleid: taint
    func() { sink(src) } ()

    // Goroutine call
    // ruleid: taint
    go func() { sink(src) } ()

    // Goroutine call via a variable
    // ruleid: taint
    r := func () { sink(src) }
    go r()

    // Goroutine call with arg requires taint-intrafile!
    // todo: taint
    // go func(x string) { sink(x) } (src)
    
    // Control test: normal lambda call
    // ruleid: taint
    func() { sink(src) } ()
}
