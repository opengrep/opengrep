pragma solidity ^0.8.0;

import "./Bases.sol";

function source() pure returns (uint256) {
    return 1;
}

contract D is A, B {
    function f(uint256 x) internal pure override(A, B) returns (uint256) {
        return super.f(x);
    }

    function run() public pure returns (uint256) {
        return f(source());
    }
}
