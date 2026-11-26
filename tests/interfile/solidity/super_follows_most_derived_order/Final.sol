pragma solidity ^0.8.0;

import "./Bases.sol";

function source() pure returns (uint256) {
    return 1;
}

contract Final is B1, B2 {
    function run() public pure returns (uint256) {
        return f(source());
    }

    function f(uint256 x) internal pure override(B1, B2) returns (uint256) {
        return super.f(x);
    }
}
