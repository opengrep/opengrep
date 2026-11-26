pragma solidity ^0.8.0;

function sink(uint256 x) pure returns (uint256) {
    return x;
}

function g(uint256 a, uint256 b) pure returns (uint256) {
    // ruleid: free-function-overloads-in-one-file
    return sink(a) + b;
}

function g(uint256 a) pure returns (uint256) {
    // ok: free-function-overloads-in-one-file
    return sink(a);
}

function source() pure returns (uint256) {
    return 1;
}

contract Main {
    function run() public pure returns (uint256) {
        return g(source(), 1);
    }
}
