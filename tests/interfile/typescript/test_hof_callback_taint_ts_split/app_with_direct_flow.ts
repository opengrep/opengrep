function app_with_direct_flow(f: (x: string) => string, x: string): string {
    return f(x) + x;
}
