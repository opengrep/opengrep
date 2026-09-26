import argparse
import subprocess


def run(cmd):
    # ruleid: trace_flag_in_dispatched_callee_python
    return subprocess.run(cmd)


def pip(*args):
    return ["pip", *args]


def resolves(spec):
    res = run(pip("install", "--dry-run", spec))
    return res.returncode == 0


def cmd_switch(args):
    from_source = False
    if args.from_source:
        src = args.from_source
        spec = src
        from_source = True
    else:
        spec = args.version
    if from_source and not args.editable:
        build = ["build", spec]
        if run(build).returncode != 0:
            return 1
    elif from_source:
        print("editable")
    else:
        if not resolves(spec):
            return 1
    run(pip("install", spec))
    return 0


def cmd_doctor(args):
    return 0


def main():
    parser = argparse.ArgumentParser()
    args = parser.parse_args()
    return {"doctor": cmd_doctor, "switch": cmd_switch}[args.command](args)
