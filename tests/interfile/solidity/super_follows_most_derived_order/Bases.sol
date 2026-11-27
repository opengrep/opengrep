pragma solidity ^0.8.0;

function sink(uint256 x) pure returns (uint256) {
    return x;
}

contract Base {
    function f(uint256 x) internal pure virtual returns (uint256) {
        return x;
    }
}

contract B1 is Base {
    function f(uint256 x) internal pure virtual override returns (uint256) {
        // ruleid: super-follows-most-derived-order
        return sink(x);
    }
}

contract B2 is Base {
    function f(uint256 x) internal pure virtual override returns (uint256) {
        return super.f(x);
    }
}
