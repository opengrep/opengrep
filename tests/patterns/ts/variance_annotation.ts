// MATCH:
interface Producer<out T> {
  get(): T;
}
interface Consumer<in T> {
  put(x: T): void;
}
