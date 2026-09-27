import java.util.List;

class RunnableChain {
  void chain(List<String> items) {
    Runnable r = () -> {};
    for (String item : items) {
      Runnable prev = r;
      // ruleid: runnable_chain_by_value_java
      r = () -> { prev.run(); sink(source()); };
    }
    r.run();
  }
}
