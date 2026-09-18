from outer3_yes import outer3_yes
from source import source

def call_chain3_yes():
    outer3_yes({"data": [1, 2]}, "f", source())
