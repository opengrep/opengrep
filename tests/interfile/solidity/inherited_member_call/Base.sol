pragma solidity ^0.8.0;

function sink(uint256 x) pure returns (uint256) {
    return x;
}

contract Base {
    function leak(uint256 x) internal pure returns (uint256) {
        // ruleid: test-inherited-member-call
        return sink(x);
    }

    function keep(uint256 x) internal pure returns (uint256) {
        // ok: test-inherited-member-call
        return sink(x);
    }
}
