pragma solidity ^0.8.0;

function _leak(uint256 x) pure returns (uint256) {
    // ruleid: path-import-binds-underscore-name
    return sink(x);
}
