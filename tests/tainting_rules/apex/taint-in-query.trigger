trigger myTrig on Account (before Insert)
{
    String userData = unsafeGetData();

    // ruleid: taint-in-query-test
    sink(userData);

    // ruleid: taint-in-query-test
    [SELECT c1, c2 FROM t1 WHERE c1 = :userData AND c2 = 100 LIMIT 10];
    
    // ok
    [SELECT c1, c2 FROM t1 WHERE c1 = :otherData AND c2 = 100 LIMIT 10];
}
