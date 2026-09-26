<?php
trait T1 { public function handle($x) { } }
trait T2 { public function handle($x) { } }

//ERROR:
class C {
    use T1, T2 {
        T1::handle insteadof T2;
        T2::handle as protected quiet;
    }
}

class D {
    use T1, T2 {
        T1::handle insteadof T2;
    }
}

class E {
    use T1, T2 {
        T1::handle insteadof T2;
        T2::handle as loud;
    }
}
