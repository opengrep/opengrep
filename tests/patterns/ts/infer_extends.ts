// MATCH:
type First<X> = X extends [infer H extends string, ...unknown[]] ? H : never;
type Second<X> = X extends [infer H extends number, ...unknown[]] ? H : never;
