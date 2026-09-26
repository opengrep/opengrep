<?php
trait T { public function handle($x) { } }

class Base { use T; }

class C extends Base {
    use T {
        self::handle as quiet;
    }
}

//ERROR:
class D extends Base {
    use T {
        parent::handle as quiet;
    }
}

class E extends Base {
    use T {
        static::handle as quiet;
    }
}
