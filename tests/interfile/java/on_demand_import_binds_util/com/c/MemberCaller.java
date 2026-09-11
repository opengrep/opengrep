package com.c;

import com.a.Util.*;

class MemberCaller {
    static String source() {
        return System.getenv("SECRET");
    }

    public static void main(String[] args) {
        relay(source());
    }
}
