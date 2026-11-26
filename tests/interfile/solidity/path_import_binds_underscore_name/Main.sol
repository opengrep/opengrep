pragma solidity ^0.8.0;

import "./Lib.sol";

contract Main {
    function run() public pure returns (uint256) {
        return _leak(source());
    }
}
