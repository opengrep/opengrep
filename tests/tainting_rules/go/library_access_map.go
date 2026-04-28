package main

func handlerWritePos() {
	m := map[string]string{}
	m["body"] = source()
	m["user"] = "safe"
	// ruleid: taint
	sink(m["body"])
}

func handlerWriteNeg() {
	m := map[string]string{}
	m["body"] = "safe"
	m["user"] = source()
	// ok: taint
	sink(m["body"])
}

func handlerCommaOkPos() {
	m := map[string]string{}
	m["body"] = source()
	v, ok := m["body"]
	_ = ok
	// ruleid: taint
	sink(v)
}

func handlerCommaOkNeg() {
	m := map[string]string{}
	m["body"] = "safe"
	m["user"] = source()
	v, ok := m["body"]
	_ = ok
	// ok: taint
	sink(v)
}

func handlerDeleteClearsCell() {
	m := map[string]string{}
	m["body"] = source()
	delete(m, "body")
	// ok: taint
	sink(m["body"])
}

func handlerDeleteSiblingKeepsTaint() {
	m := map[string]string{}
	m["body"] = source()
	delete(m, "user")
	// ruleid: taint
	sink(m["body"])
}
