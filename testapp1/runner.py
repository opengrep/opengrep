import subprocess


def run_cmd(cmd):
    subprocess.run(cmd, shell=True)
