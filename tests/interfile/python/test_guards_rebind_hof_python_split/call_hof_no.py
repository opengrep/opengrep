from do_sink_no import do_sink_no
from outer_hof_no import outer_hof_no
from source import source

def call_hof_no():
    outer_hof_no("c", "d", do_sink_no, "e", [1], "f", source())


