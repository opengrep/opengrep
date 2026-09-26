import argparse
import subprocess


def switch(opts, data):
    if opts.alpha:
        spec = data.alpha
    else:
        spec = data.beta
    if opts.alpha:
        subprocess.run(["tool", "--done"])
    else:
        # ruleid: trace_condition_on_parameter_python
        subprocess.run(["tool", spec])


def entry(opts):
    data = argparse.ArgumentParser().parse_args()
    switch(opts, data)
