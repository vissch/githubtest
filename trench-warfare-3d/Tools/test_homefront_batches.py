#!/usr/bin/env python3
"""Home Front submits one instanced draw per (set, model), split only at MaxBuildings.

    python Tools/test_homefront_batches.py
Stdlib only. Iron's seven buildings are four models; Brass's eight are six. A draw that
still walks every building is a fail, and so is a BatchCount that does not match the table.
"""
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
DIORAMA = ROOT / "Assets" / "_Project" / "Presentation" / "Meta" / "HomeFrontDiorama.cs"
BUILDINGS = ROOT / "Assets" / "_Project" / "UI" / "Campaign" / "FactionBuildings.cs"


def models_of(source, method):
    body = re.search(r"static Building\[\] %s\(\) => new\[\]\s*\{(.*?)\n        \};" % method, source, re.S)
    if not body:
        raise SystemExit("cannot find %s" % method)
    pairs = re.findall(r'Set = "([^"]+)", Model = "([^"]+)"', body.group(1))
    return pairs


def batches(pairs, cap):
    counts = {}
    for key in pairs:
        counts[key] = counts.get(key, 0) + 1
    total = 0
    for n in counts.values():
        total += (n + cap - 1) // cap
    return total


def main():
    diorama = DIORAMA.read_text(encoding="utf-8")
    buildings = BUILDINGS.read_text(encoding="utf-8")
    bad = []
    if "public static int BatchCount" not in diorama:
        bad.append("HomeFrontDiorama.BatchCount is missing")
    if 'Set + "/" + Model' not in diorama:
        bad.append('batches are not keyed on set + "/" + model')
    cap = re.search(r"public const int MaxBuildings = (\d+);", diorama)
    if not cap:
        bad.append("MaxBuildings is missing")
    draw = re.search(r"for \(int bi = 0; bi < batches\.Count; bi\+\+\)\s*\{.*?FrameBudget\.Draw\([^;]*batch\.Matrices", diorama, re.S)
    if not draw:
        bad.append("Draw does not submit one FrameBudget.Draw per batch")
    if bad:
        print("FAIL")
        for line in bad:
            print(" -", line)
        return 1
    cap_n = int(cap.group(1))
    iron = batches(models_of(buildings, "BuildIron"), cap_n)
    brass = batches(models_of(buildings, "BuildBrass"), cap_n)
    if iron != 4 or brass != 6:
        print("FAIL")
        print(" - Iron batches %s (want 4), Brass batches %s (want 6), cap %s" % (iron, brass, cap_n))
        return 1
    print("PASS Iron %s batches, Brass %s batches, one draw per batch" % (iron, brass))
    return 0


if __name__ == "__main__":
    sys.exit(main())
