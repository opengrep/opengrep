// A wildcard inside a selector group is the wildcard import
// MATCH:
import a.{_}

// MATCH:
import a._

// A named selector is not a wildcard
// OK:
import a.{C}
