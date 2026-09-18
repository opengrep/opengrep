from do_sink_yes import do_sink_yes
from outer_hof_yes import outer_hof_yes
from source import source

def call_hof_yes():
    outer_hof_yes("c", "d", do_sink_yes, "e", [1, 2], "f", source())
