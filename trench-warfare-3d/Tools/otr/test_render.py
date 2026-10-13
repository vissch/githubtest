#!/usr/bin/env python3
"""[O12] [O13] builds and runs test_render/ (a standalone compile of Render.cs + stand-ins for the other otr files)
and checks every printed line of each id is PASS. python Tools/otr/test_render.py"""
import shutil, subprocess, sys
from pathlib import Path

HERE = Path(__file__).resolve().parent / "test_render"
DOTNET = shutil.which("dotnet") or r"C:\Program Files\dotnet\dotnet.exe"
IDS = ("[O12]", "[O13]")


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
