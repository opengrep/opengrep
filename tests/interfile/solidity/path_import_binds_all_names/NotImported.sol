pragma solidity ^0.8.0;

import "./Sink.sol";

function relay(uint256 x) pure returns (uint256) {
    // ok: test-path-import
    return sink(x);
}
