import argparse
import subprocess


def main():
    args = argparse.ArgumentParser().parse_args()
    built = False
    if args.alpha:
        spec = args.alpha
        built = True
    else:
        chosen = args.beta
        spec = chosen
    if built:
        subprocess.run(["tool", "--done"])
    else:
        # ruleid: trace_flag_python
        subprocess.run(["tool", spec])
