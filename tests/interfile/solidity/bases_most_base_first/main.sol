pragma solidity ^0.8.0;

contract X {
    function handle(uint x) public {
        // ok: bases-most-base-first
        sink(x);
    }
}

contract A is X {
    function handle(uint x) public override {
        // ruleid: bases-most-base-first
        sink(x);
    }
}

contract B is X, A {
    function run() public {
        handle(source());
    }
}
