// A bare call in a contract calls the contract's function.
contract A {
    function snk(uint y) internal {
        // ruleid: solidity_bare_call_of_contract_function
        sink(y);
    }
    function clean(uint y) internal returns (uint) {
        return 1;
    }
    function run() public {
        snk(source());
        // ok: solidity_bare_call_of_contract_function
        sink(clean(source()));
    }
}
