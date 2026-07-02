export class Store {
    data: string;

    constructor(data: string) {
        this.data = data;
    }

    send(): void {
        // ruleid: test-constructor-taint
        sink(this.data);
    }
}

function sink(x: string): void {
    console.log(x);
}
