trigger myTrig on Account (before Insert)
{
    // ERROR:
    [SELECT col FROM bankAccount LIMIT :userData];

    // ERROR:
    [SELECT userData, otherData FROM bankAccount];

    // OK:
    [SELECT userData1 FROM bankAccount];
    
    // ERROR:
    [SELECT c1, c2 FROM t1 WHERE c1 = :userData];
    
    // OK:
    [SELECT c1, c2 FROM t1 WHERE c1 = userData];

    // ERROR:
    [SELECT c1, c2 FROM t1 WHERE c1 = :userData + 17 AND c2 > 2 LIMIT 100];
}
