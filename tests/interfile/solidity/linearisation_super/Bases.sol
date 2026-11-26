pragma solidity ^0.8.0;

function sink(uint256 x) pure returns (uint256) {
    return x;
}

contract A {
    function f(uint256 x) internal pure virtual returns (uint256) {
        // ok: test-linearisation-super
        return sink(x);
    }
}

contract B {
    function f(uint256 x) internal pure virtual returns (uint256) {
        // ruleid: test-linearisation-super
        return sink(x);
    }
}
