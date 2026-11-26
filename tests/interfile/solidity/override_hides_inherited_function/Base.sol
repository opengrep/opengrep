pragma solidity ^0.8.0;

function sink(uint256 x) pure returns (uint256) {
    return x;
}

contract Base {
    function g(uint256 a) internal pure virtual returns (uint256) {
        // ok: override-hides-inherited-function
        return sink(a);
    }
}
