module 0x1::m {
    const MyConstant: u64 = 10;

    fun laterArmReachable(k: u64): u64 {
        let x = taint_source();
        match (k) {
            MyConstant => 0,
            // ruleid: taint-switch-case-move-on-aptos
            _ => sink(x),
        }
    }

    fun labelIsNotBound(): u64 {
        let k = taint_source();
        match (k) {
            // ok: taint-switch-case-move-on-aptos
            MyConstant => sink(MyConstant),
            _ => 0,
        }
    }

    fun binderStillBinds(): u64 {
        let k = taint_source();
        match (k) {
            MyConstant => 0,
            // ruleid: taint-switch-case-move-on-aptos
            other => sink(other),
        }
    }

    fun armsDoNotFallThrough(k: u64): u64 {
        let y = 0;
        match (k) {
            MyConstant => { y = taint_source(); 0 },
            // ok: taint-switch-case-move-on-aptos
            _ => sink(y),
        }
    }

    fun breakLeavesTheLoop(k: u64): u64 {
        let y = 0;
        while (more()) {
            match (k) {
                MyConstant => { y = taint_source(); break },
                _ => {},
            };
            y = 0;
        };
        // ruleid: taint-switch-case-move-on-aptos
        sink(y)
    }

    fun breakSkipsTheRestOfTheLoop(k: u64) {
        while (more()) {
            let y = 0;
            match (k) {
                MyConstant => { y = taint_source(); break },
                _ => {},
            };
            // ok: taint-switch-case-move-on-aptos
            sink(y);
        };
    }
}
