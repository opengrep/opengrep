import sinks

int source() { result = 1 }

int run() { result = relay(source()) + quiet(2) }
