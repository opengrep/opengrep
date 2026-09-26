import sys


def load(path, definitions):
    # ruleid: loop_variable_rebound_to_record_python
    open(path)
    for defn_dict in definitions:
        specification = defn_dict.pop("name")
        defn_dict = {"name": specification, "dispatch": {"Default": defn_dict}}


def main():
    load(sys.argv[1], [])
