import argparse
import subprocess


def switch(args):
    built = False
    if args.zeta:
        spec = args.zeta
        built = True
    else:
        spec = args.beta
    if built:
        subprocess.run(["tool", "--done"])
    else:
        # ruleid: trace_flag_in_callee_other_order_python
        subprocess.run(["tool", spec])


def main():
    parser = argparse.ArgumentParser()
    args = parser.parse_args()
    switch(args)
