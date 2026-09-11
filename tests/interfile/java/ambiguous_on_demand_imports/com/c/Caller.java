package com.c;

import com.a.*;
import com.b.*;

class Caller {
    static String source() {
        return System.getenv("SECRET");
    }

    public static void main(String[] args) {
        Util.run(source());
    }
}
