struct Target {};

struct Holder {
    //ERROR: match
    operator Target() const { return Target(); }

    //ERROR: match
    explicit operator bool() const { return true; }

    Target convert() const { return Target(); }

    Holder operator+(const Holder &other) const { return other; }
};
