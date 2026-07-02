import { Store } from "./Store";

function source(): string {
    return process.env.SECRET!;
}

function main(): void {
    const tainted = source();
    const s = new Store(tainted);
    s.send();
}

main();
