import java.util.List;
import java.util.function.Function;

class ClosureChain {
  void chain(List<Function<String, String>> gs) {
    Function<String, String> f = s -> s;
    for (Function<String, String> g : gs) {
      Function<String, String> prev = f;
      f = s -> g.apply(prev.apply(s));
    }
    // todoruleid: closure_chain_by_value_java
    sink(f.apply(source()));
  }

  void straight(Function<String, String> g) {
    Function<String, String> prev = s -> s;
    Function<String, String> f = s -> g.apply(prev.apply(s));
    // todoruleid: closure_chain_by_value_java
    sink(f.apply(source()));
  }
}
