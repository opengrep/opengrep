// Resolving a fluent chain of calls on a receiver whose class is not known
// costs time linear in the length of the chain.
class FluentChain {
    void run(StringBuilder b) {
        // ruleid: fluent_chain_java
        sink(b
            .append(1)
            .append(2)
            .append(3)
            .append(4)
            .append(5)
            .append(6)
            .append(7)
            .append(8)
            .append(9)
            .append(10)
            .append(11)
            .append(12)
            .append(13)
            .append(14)
            .append(15)
            .append(16)
            .append(17)
            .append(18)
            .append(19)
            .append(20)
            .append(21)
            .append(22)
            .append(23)
            .append(24)
            .append(25)
            .append(26)
            .append(27)
            .append(28)
            .append(29)
            .append(30)
            .append(31)
            .append(32)
            .append(33)
            .append(34)
            .append(35)
            .append(36)
            .append(37)
            .append(38)
            .append(39)
            .append(40)
            .append(source()));
    }
}
