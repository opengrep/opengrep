// A class pattern matches classes and extensions (parsed as classes), not
// structs, which are a distinct kind of type

// MATCH:
class Foo {}
struct Foo {}
// MATCH:
extension Foo {}
