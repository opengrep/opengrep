pragma solidity ^0.8.0;

contract D is B {
    function f(uint256 x) internal override {
        //ERROR: match
        super.f(x);
        //ERROR: match
        b.f(x);
        f(x);
        super.g(x);
    }
}
