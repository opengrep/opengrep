package main

func byValue(o T) { o.F = source() }

func byPointer(o *T) { o.F = source() }

func (t T) setV() { t.F = source() }

func (t *T) setP() { t.F = source() }
