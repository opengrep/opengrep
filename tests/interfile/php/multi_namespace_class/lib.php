<?php
// Name collision: TWO classes both named `Base`, one per namespace region.
// `A\Base::handle` is benign; `B\Base::handle` sinks.  Only the per-region qn
// (`A.Base` vs `B.Base`) lets `Client extends B\Base` resolve its parent to
// the sink class through the MRO — leaf-name dispatch alone sees two `handle`
// methods and builds no edge.
namespace A {
    class Base {
        function handle($x) {
            // ok: multi-namespace-class
            return $x;
        }
    }
}

namespace B {
    class Base {
        function handle($x) {
            // ruleid: multi-namespace-class
            sink($x);
        }
    }
}
