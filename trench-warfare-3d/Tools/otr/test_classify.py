#!/usr/bin/env python3
"""[O9] [O11] builds and runs test_classify/ (a standalone compile of Runner.cs + a fake engine stub) and checks
every printed PASS/FAIL line of each id. python Tools/otr/test_classify.py"""
import shutil, subprocess, sys
from pathlib import Path

HERE = Path(__file__).resolve().parent / "test_classify"
DOTNET = shutil.which("dotnet") or r"C:\Program Files\dotnet\dotnet.exe"
IDS = ("[O9]", "[O11]")


def main():
    r = subprocess.run([DOTNET, "run", "--project", str(HERE)], capture_output=True, text=True)
    lines = [l for l in r.stdout.splitlines() if l.startswith(IDS)]
    if not lines:
        print(r.stdout)
        print(r.stderr, file=sys.stderr)
        print("FAIL: dotnet run did not produce %s lines (exit %s)" % ("/".join(IDS), r.returncode))
        return 1
    ok = all(l.split("] ", 1)[1].startswith("PASS") for l in lines)
    ok = ok and all(any(l.startswith(i) for l in lines) for i in IDS)   # every id has to have run
    for l in lines:
        print(l)
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
