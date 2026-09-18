// Cross-function field-sensitive taint through Kotlin's
// kotlin.collections.MutableMap library-call recognisers. Maps flow
// across function boundaries as parameters / returns, and field-
// sensitivity must survive the call. Also exercises [getOrElse] —
// the trailing-lambda form — whose lambda return requires the
// intrafile analyser.









