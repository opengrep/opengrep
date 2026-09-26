class R {
    var path: String
    var other: String

    init(path: String, other: String) {
        self.path = path
        self.other = other
    }
}

func tainted_first() {
    let arr = [source(), "x"]
    // ruleid: collection_element_property_swift
    sink(arr.first)
}

func clean_array() {
    let arr = ["x", "y"]
    // ok: collection_element_property_swift
    sink(arr.last)
}

func element_fields() {
    var infos = [R]()
    infos.append(R(path: source(), other: "x"))
    let e = infos.first
    // ruleid: collection_element_property_swift
    sink(e?.path)
    // ok: collection_element_property_swift
    sink(e?.other)
}

func last_fields() {
    var infos = [R]()
    infos.append(R(path: source(), other: "x"))
    let e = infos.last
    // ruleid: collection_element_property_swift
    sink(e?.path)
    // ok: collection_element_property_swift
    sink(e?.other)
}
