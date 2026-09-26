import argparse
import subprocess


def main():
    args = argparse.ArgumentParser().parse_args()

    def finish():
        nonlocal done
        done = True

    if args.fast:
        done = True
        spec = "fixed"
    else:
        done = False
        spec = args.alpha
    finish()
    if done:
        # ruleid: trace_flag_set_by_closure_python
        subprocess.run(["tool", spec])
