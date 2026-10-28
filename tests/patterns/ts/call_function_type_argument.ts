// MATCH:
const a = useAsyncFn<(params: P) => Promise<D>>(load, []);
const b = useAsyncFn<(params: P) => Promise<D>>;
