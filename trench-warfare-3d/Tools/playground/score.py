# The scoreboard for capture rounds: every measured number from a round in one table, and against the round before it
# with a regression flag wherever a number moved the wrong way by more than its measured noise.
#   python Tools/playground/score.py TAG [PREV] [DIR]   -> DIR/TAG_scores.json; prints the table and REGRESSED lines
# Noise floors, measured on unchanged builds (docs/22; loop 2): pop IoU 0.006; block colour 0.6 for the tank and frog
# and 1.0 for the new machines (a gunship read 0.2-0.97 three times running; the frog's 2->3 swings 2-6 and is not
# flagged); m3 contrast is the mean of nine frames (single frames run 0.23-0.47 on one build, in two clusters) with a floor of 0.08; side cross-talk
# 0.002. Frames per second are the editor's, swayed by whatever else the machine runs: shown, never flagged.
import sys, json, os, glob
T = sys.argv[1]; PREV = sys.argv[2] if len(sys.argv) > 2 and sys.argv[2] != "-" else None
D = sys.argv[3] if len(sys.argv) > 3 else os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..", "Captures", "playground")

def pops(path, tag):
    out = {}
    if not os.path.exists(path): return out
    d = json.load(open(path))["pops"]
    for f in sorted({p["from"] for p in d}):
        s = [p for p in d if p["from"] == f]
        out["%s %d->%d iou" % (tag, f, f + 1)] = round(min(p["iou"] for p in s), 3)
        out["%s %d->%d block" % (tag, f, f + 1)] = round(sum(p["dblock"] for p in s) / len(s), 1)
    return out

def read(t):
    sc = {}
    sc.update(pops(f"{D}/{t}_pop_vehicle.json", "brute"))
    sc.update(pops(f"{D}/{t}_pop_unit.json", "frog"))
    for n in ("croaker", "hopper", "mercy", "skimmer"): sc.update(pops(f"{D}/{t}_pop_{n}.json", n))
    p = f"{D}/{t}_m3_sidehue.json"
    if os.path.exists(p):
        j = json.load(open(p))
        sc["side cross-talk"] = round(max(j.get("side_cross", [0])), 4); sc["men hue gap"] = j.get("men_gap")
    for shot, tag in (("m3_mud_standard", "m3"), ("u9_squad", "u9")):
        files = [f"{D}/{t}_{shot}.json"] + (sorted(glob.glob(f"{D}/{t}_m3c_*.json")) if tag == "m3" else [])
        vals = sorted(json.load(open(f))["contrast_median"] for f in files if os.path.exists(f) and "contrast_median" in json.load(open(f)))
        # the MEAN: the frames fall in two clusters (~0.24 and ~0.40 on one build) and a median jumps between them as
        # the split goes 5/4 or 4/5 (r41 0.373 -> r42 0.269, nothing changed that touches the men or the ground)
        if vals: sc[tag + " contrast"] = round(sum(vals) / len(vals), 3)
    # frames per second as the round saw them (a big drop means something got expensive)
    fps = [json.load(open(f)).get("fps", 0) for f in glob.glob(f"{D}/{t}_*.json") if not os.path.basename(f).startswith(f"{t}_pop") and "fps" in open(f).read(200)]
    if fps: sc["fps median"] = sorted(fps)[len(fps) // 2]
    return sc

# which way is better, and how much a number may wander on an unchanged build
RULE = {"iou": (+1, 0.006), "block": (-1, 0.6), "contrast": (+1, 0.08), "cross-talk": (-1, 0.002), "gap": (+1, 5), "fps": (0, 0)}
def rule(k):
    if "block" in k and k.split()[0] in ("croaker", "hopper", "mercy", "skimmer"): return (-1, 1.0)
    for key, r in RULE.items():
        if key in k: return r
    return (0, 0)

sc = read(T)
json.dump(sc, open(f"{D}/{T}_scores.json", "w"), indent=1)
prev = json.load(open(f"{D}/{PREV}_scores.json")) if PREV and os.path.exists(f"{D}/{PREV}_scores.json") else (read(PREV) if PREV else {})
bad = []
print("%-28s %10s %10s" % ("score", T, PREV or ""))
for k in sorted(set(sc) | set(prev)):
    a, b = sc.get(k), prev.get(k)
    flag = ""
    if a is not None and b is not None:
        sign, noise = rule(k)
        # the frog's 2->3 block colour swings 2-6 on one build (lodfit): not a signal
        if sign and not (k.startswith("frog 2->3 block")) and (a - b) * sign < -noise: flag = "  REGRESSED"; bad.append(k)
        elif sign and (a - b) * sign > noise: flag = "  better"
    print("%-28s %10s %10s%s" % (k, a, b, flag))
print("REGRESSIONS: %s" % (", ".join(bad) if bad else "none"))
