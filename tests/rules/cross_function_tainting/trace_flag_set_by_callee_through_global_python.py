import argparse
import subprocess

DONE = False


def finish():
    global DONE
    DONE = True


def main():
    global DONE
    args = argparse.ArgumentParser().parse_args()
    if args.fast:
        DONE = True
        spec = "fixed"
    else:
        DONE = False
        spec = args.alpha
    finish()
    if DONE:
        # ruleid: trace_flag_set_by_callee_through_global_python
        subprocess.run(["tool", spec])
