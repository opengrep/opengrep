package Foo

type Config struct {
    Name string
}

type Counter struct {
    Name int
}

func sink(s string) {}

func main() {
    cfg := Config{}
    cnt := &Counter{}
    //ERROR:
    sink(cfg.Name)
    sink(cnt.Name)
}
