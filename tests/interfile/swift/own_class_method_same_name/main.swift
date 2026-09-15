class Holder {
    func leak(_ v: String) {
        print(v)
    }
}

func main() {
    leak(source())
}
