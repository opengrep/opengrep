struct Store {
    void send(const char *input);
    void handle(const char *input) { this->send(input); }
};

struct Other {
    void send(const char *input);
};
