typedef int T;

template <typename T> struct A {
  T f(T x);
};

template <typename T> T A<T>::f(T x) {
  return x;
}

T g(T x) {
  return x;
}

template <typename T> [[gnu::aligned(sizeof(T))]] T h(T x) {
  return x;
}
