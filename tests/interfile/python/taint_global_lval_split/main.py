from intermediate import intermediate
from state_module import do_sink


def main():
    intermediate()  # Cross-file call that taints the module global
    do_sink()       # Cross-file call that sinks the module global
