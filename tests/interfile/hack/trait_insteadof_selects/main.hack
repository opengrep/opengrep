<?hh
trait T1 {
    public function handle($x): void {
        // ruleid: trait-insteadof-selects
        sink($x);
    }
}

trait T2 {
    public function handle($x): void {
        // ok: trait-insteadof-selects
        sink($x);
    }
}

class C {
    use T1, T2 {
        T1::handle insteadof T2;
    }
}

function run(): void {
    $c = new C();
    $c->handle(source());
}
