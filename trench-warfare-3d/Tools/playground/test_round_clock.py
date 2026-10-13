#!/usr/bin/env python3
"""test_round_clock.py: round.sh's destruction stills keep their own clock, whatever the bangs do.

P3.2: a bang.sh call that blocks sits between "seq" and the v2..v5 stills, so a missed bang (40 s of waiting) pushes
every one of them late and the round no longer compares with the rounds before it. The bangs are armed (non-blocking)
inside that block and joined after v5, and the sleeps add up to the clock of commit b7d4d0ede, the round before this
lane: 2.9 / 5.5 / 11.5 / 23.5 s after "seq".
"""
import io
import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ROUND = os.path.join(HERE, "round.sh")

# seconds after `do "seq"`, from b7d4d0ede:trench-warfare-3d/Tools/playground/round.sh
WANT = {"v2_hits": 2.9, "v3_burning": 5.5, "v4_cookoff": 11.5, "v5_aftermath": 23.5}


def block():
    lines = io.open(ROUND, encoding="utf-8").read().splitlines()
    start = next(i for i, l in enumerate(lines) if re.search(r'do\s+"seq"', l))
    end = next(i for i, l in enumerate(lines) if "v5_aftermath" in l)
    return lines[start:end + 1]


def main():
    b = block()
    bad = [l.strip() for l in b if "bang.sh" in l and "--arm" not in l]
    assert not bad, "a bang.sh call in the destruction block does not --arm: %r" % bad

    clock, t = {}, 0.0
    for l in b:
        for s in re.findall(r'\bsleep\s+([0-9.]+)', l):
            t += float(s)
        m = re.search(r'shot\s+\$\{T\}_(\w+)', l)
        if m:
            clock[m.group(1)] = round(t, 3)

    for name, want in WANT.items():
        assert name in clock, "no still %s in the block" % name
        assert abs(clock[name] - want) < 1e-6, \
            "%s is %.3f s after seq, not the round before this lane's %.3f" % (name, clock[name], want)

    print("round clock ok: " + ", ".join("%s=%.1f" % (k, clock[k]) for k in sorted(WANT)))
    return 0


if __name__ == "__main__":
    sys.exit(main())
