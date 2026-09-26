struct A {};

//ERROR:
struct L : public virtual A {};

struct M : virtual A {};

struct R : public A {};
