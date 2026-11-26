typedef int Ts;

// ERROR: match
template <typename... Ts> void g(Ts... xs) {}

template <typename... Us> void gNeg(Ts... xs) {}
