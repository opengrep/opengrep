class InternalSource {
    String data;

    InternalSource() {
        this.data = source();
    }

    String getData() {
        return this.data;
    }
}
