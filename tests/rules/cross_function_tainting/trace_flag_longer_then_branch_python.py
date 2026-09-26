import argparse
import subprocess


def main():
    args = argparse.ArgumentParser().parse_args()
    built = False
    if args.alpha:
        first = args.alpha
        second = first
        spec = second
        built = True
    else:
        spec = args.beta
    if built:
        subprocess.run(["tool", "--done"])
    else:
        # ruleid: trace_flag_longer_then_branch_python
        subprocess.run(["tool", spec])
