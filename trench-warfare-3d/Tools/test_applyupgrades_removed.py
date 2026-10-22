#!/usr/bin/env python3
"""The upgrade tiers in CampaignProfile have no bake path, so DataBaker must not keep ApplyUpgrades.

    python Tools/test_applyupgrades_removed.py
Stdlib only. The accuracy clamp (a hit chance stays at most 1) is the rule a future SHOW wiring has to keep;
tasks.md states it, because the method that used to enforce it was never called.
"""
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
REPO = ROOT.parent
BAKER = ROOT / "Assets" / "_Project" / "Data" / "DataBaker.cs"
TESTS = ROOT / "Assets" / "_Project" / "Tests" / "Sim" / "DataBakerTests.cs"
TASKS = REPO / "docs" / "reference" / "tasks.md"
SIG = "public static UnitStats ApplyUpgrades("


def main():
    baker = BAKER.read_text(encoding="utf-8")
    tests = TESTS.read_text(encoding="utf-8")
    tasks = TASKS.read_text(encoding="utf-8")
    bad = []
    if SIG in baker:
        bad.append("DataBaker.cs still declares ApplyUpgrades")
    if "ApplyUpgrades(" in tests:
        bad.append("DataBakerTests.cs still calls ApplyUpgrades")
    if "clamps at 1" not in tasks:
        bad.append("tasks.md does not state that accuracy clamps at 1")
    if bad:
        print("FAIL")
        for line in bad:
            print(" -", line)
        return 1
    print("PASS ApplyUpgrades is gone; accuracy still clamps at 1 when a path is wired")
    return 0


if __name__ == "__main__":
    sys.exit(main())
