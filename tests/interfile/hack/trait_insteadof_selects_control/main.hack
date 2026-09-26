<?hh
trait T1 {
    public function handle($x): void {
        // ruleid: trait-insteadof-selects-control
        sink($x);
    }
}

class C {
    use T1;
}

function run(): void {
    $c = new C();
    $c->handle(source());
}
