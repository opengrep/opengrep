pragma solidity ^0.8.0;

import "./Base.sol";

function source() pure returns (uint256) {
    return 1;
}

contract D is Base {
    function g(uint256 a) internal pure returns (uint256) {
        return a;
    }

    function run() public pure returns (uint256) {
        return g(source(), 1);
    }
}
