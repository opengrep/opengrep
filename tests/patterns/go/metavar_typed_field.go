package Foo

type Config struct {
    HomePath string
    Count    int
}

func sink(s string) {}

func main() {
    cfg := Config{}
    //ERROR:
    sink(cfg.HomePath)
    sink(cfg.Count)
}
