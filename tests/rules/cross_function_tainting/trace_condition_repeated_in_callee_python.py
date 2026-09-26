import argparse
import subprocess


def switch(args):
    if args.alpha:
        spec = args.alpha
    else:
        spec = args.beta
    if args.alpha:
        subprocess.run(["tool", "--done"])
    else:
        # ruleid: trace_condition_repeated_in_callee_python
        subprocess.run(["tool", spec])


def main():
    parser = argparse.ArgumentParser()
    args = parser.parse_args()
    switch(args)
