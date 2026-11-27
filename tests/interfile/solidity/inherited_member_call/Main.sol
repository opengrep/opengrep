pragma solidity ^0.8.0;

import "./Base.sol";

function source() pure returns (uint256) {
    return 1;
}

contract Main is Base {
    function run() public pure returns (uint256) {
        return leak(source()) + keep(2);
    }
}
