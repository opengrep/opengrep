function app_callback_only<T, R>(f: (x: T) => R, x: T): R {
    return f(x);
}
