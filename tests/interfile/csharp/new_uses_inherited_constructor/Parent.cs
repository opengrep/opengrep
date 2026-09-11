namespace A {
    class Parent {
        protected string data;

        public Parent(string x) {
            this.data = x;
        }

        public void Report() {
            // ruleid: new-uses-inherited-constructor
            sink(this.data);
        }
    }
}
