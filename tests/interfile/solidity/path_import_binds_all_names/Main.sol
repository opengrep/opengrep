pragma solidity ^0.8.0;

import "./Imported.sol";

function source() pure returns (uint256) {
    return 1;
}

contract Main {
    function run() public pure returns (uint256) {
        return relay(source());
    }
}
