pragma solidity ^0.8.0;

contract D is B {
    function f(uint256 x) internal override {
        //ERROR: match
        super.f(x);
        //ERROR: match
        super.g(x);
        b.f(x);
        f(x);
    }
}
