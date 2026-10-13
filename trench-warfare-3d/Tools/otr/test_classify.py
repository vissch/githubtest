#!/usr/bin/env python3
"""[O9] builds and runs test_classify/ (a standalone compile of Runner.cs + a fake engine stub) and checks every
printed [O9] PASS/FAIL line. python Tools/otr/test_classify.py"""
import shutil, subprocess, sys
from pathlib import Path

HERE = Path(__file__).resolve().parent / "test_classify"
DOTNET = shutil.which("dotnet") or r"C:\Program Files\dotnet\dotnet.exe"


def main():
    r = subprocess.run([DOTNET, "run", "--project", str(HERE)], capture_output=True, text=True)
    lines = [l for l in r.stdout.splitlines() if l.startswith("[O9]")]
    if r.returncode != 0 or not lines:
        print(r.stdout)
        print(r.stderr, file=sys.stderr)
        print("[O9] FAIL: dotnet run did not produce [O9] lines (exit %s)" % r.returncode)
        return 1
    ok = all(l.startswith("[O9] PASS") for l in lines)
    for l in lines:
        print(l)
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
