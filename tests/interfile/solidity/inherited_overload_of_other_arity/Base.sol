pragma solidity ^0.8.0;

function sink(uint256 x) pure returns (uint256) {
    return x;
}

contract Base {
    function g(uint256 a, uint256 b) internal pure returns (uint256) {
        // ruleid: inherited-overload-of-other-arity
        return sink(a) + b;
    }
}
