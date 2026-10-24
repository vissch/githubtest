"""Measures the blast knockback in metres, from the game's own drawing, not from pixels.

Each capture is one staging of DeathLab.Scene("shell", 35, z): six men in a row 3 m apart along x (35..50),
one 8 m / 600 damage shell at (42.5, z-2), DeathLab.Absurd(0) so no death gag takes a man and every death is a
plain blast throw. 3.5 s of game time later DeathLab.Throws writes, for every fallen body the renderer holds,
where he stood when he died (from), where he first touched down (land) and where he lies (rest), in world metres.
This probe reads those jsons: the throw of a man is how far he moved from where he stood,
hypot(rest_x - from_x, rest_z - from_z). hypot(rest - land) is the bounce/skid this unit added, printed as context.

The three jsons are the latest capture of the build. A later leg re-captures with the sequence in frames.txt
(same spots, same framing) and reruns this probe; the bar below is fixed, so a build that stops throwing bodies,
or one that flings them absurdly, fails.

It supersedes docs/evidence/blast-knockback-death/probe.py, which read one centroid for all six men out of a
before/after mask (so one number for six throws), had no bar to pass or fail, let blast debris into the "added"
mask, and calibrated off a still its own sidecar called unreadable.

Only `json` and `math`: no numpy, PIL or scipy, so it cannot fail for want of a package.
"""
import json
import math
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))

# The three captures of the current build (GreyboxCorridor, map 90 x 276 m; the row at x 35..50 so a throw never
# meets the map's edge clamp). A different spot per run: VATRenderer's Hash01 keys off the death position and the
# sim seed is fixed per load, so the same spot repeats itself exactly and the spread would be zero.
#   run 1, row z = 90:  min 6.32 m, max 14.67 m
#   run 2, row z = 150: min 7.68 m, max 12.84 m
#   run 3, row z = 190: min 5.21 m, max 17.88 m
# Observed over all 18 bodies: min 5.21 m, max 17.88 m. The bar sits outside that spread on both sides:
# LOW  = floor(5.21 * 0.8 * 10) / 10
# HIGH = ceil(17.88 * 1.25 * 10) / 10
RUNS = ((1, 90.0), (2, 150.0), (3, 190.0))
LOW = 4.1
HIGH = 22.4
WANT_BODIES = 6
NEAR_ROW_M = 1.0   # a body that died more than this off the row's z is another death on the field, not this shell's


def throw_m(body):
    """How far a man moved from where he stood, on the ground (metres)."""
    return math.hypot(body["rest_x"] - body["from_x"], body["rest_z"] - body["from_z"])


def settle_m(body):
    """The bounce and skid after he first touched down (metres)."""
    return math.hypot(body["rest_x"] - body["land_x"], body["rest_z"] - body["land_z"])


def median(xs):
    s = sorted(xs)
    n = len(s)
    return s[n // 2] if n % 2 else 0.5 * (s[n // 2 - 1] + s[n // 2])


def selfcheck():
    """The maths, on a worked 3-4-5 example, before any file is read."""
    got = throw_m({"from_x": 0.0, "from_z": 0.0, "rest_x": 3.0, "rest_z": 4.0})
    if abs(got - 5.0) > 1e-9:
        print("FAIL self-check: (0,0) -> (3,4) came out %.6f m, not 5.0" % got)
        return False
    return True


def main():
    if not selfcheck():
        return 1
    bad = []
    all_throws = []
    out = {"bar": {"low_m": LOW, "high_m": HIGH}, "runs": []}
    for k, z in RUNS:
        path = os.path.join(HERE, "throws%d.json" % k)
        if not os.path.exists(path):
            bad.append("run %d: no throws%d.json" % (k, k))
            continue
        with open(path) as f:
            doc = json.load(f)
        row = [b for b in doc["bodies"] if abs(b["from_z"] - z) <= NEAR_ROW_M]
        gagged = [b for b in row if b["gag"] != 0]
        thrown = [b for b in row if b["thrown"] and b["gag"] == 0]
        print("run %d (row at z = %g): %d bodies on the row, %d thrown" % (k, z, len(row), len(thrown)))
        bodies = []
        for b in sorted(thrown, key=lambda b: b["from_x"]):
            t, s = throw_m(b), settle_m(b)
            print("  stood at (%.1f, %.1f): threw %.2f m  (settle %.2f m)" % (b["from_x"], b["from_z"], t, s))
            bodies.append({"from_x": b["from_x"], "from_z": b["from_z"], "throw_m": round(t, 4), "settle_m": round(s, 4)})
            all_throws.append(t)
            if not LOW <= t <= HIGH:
                bad.append("run %d: the man at (%.1f, %.1f) threw %.2f m, outside %.1f..%.1f m" % (k, b["from_x"], b["from_z"], t, LOW, HIGH))
        if gagged:
            bad.append("run %d: %d body/bodies took a death gag (gag != 0); Absurd(0) was not in force" % (k, len(gagged)))
        if len(thrown) < WANT_BODIES:
            bad.append("run %d: %d thrown bodies on the row, wanted %d" % (k, len(thrown), WANT_BODIES))
        ts = [b["throw_m"] for b in bodies]
        if ts:
            print("  run %d: min %.2f  median %.2f  max %.2f m" % (k, min(ts), median(ts), max(ts)))
        out["runs"].append({"run": k, "row_z": z, "bodies": bodies,
                            "min_m": round(min(ts), 4) if ts else None,
                            "median_m": round(median(ts), 4) if ts else None,
                            "max_m": round(max(ts), 4) if ts else None})
    if len(out["runs"]) < 3:
        bad.append("fewer than 3 runs read")
    if all_throws:
        print("all %d bodies: min %.2f  median %.2f  max %.2f m; bar %.1f..%.1f m"
              % (len(all_throws), min(all_throws), median(all_throws), max(all_throws), LOW, HIGH))
        out["all"] = {"count": len(all_throws), "min_m": round(min(all_throws), 4),
                      "median_m": round(median(all_throws), 4), "max_m": round(max(all_throws), 4)}
    out["pass"] = not bad
    out["problems"] = bad
    with open(os.path.join(HERE, "probe.json"), "w") as f:
        json.dump(out, f, indent=2)
        f.write("\n")
    for line in bad:
        print("FAIL " + line)
    print("PASS: every body's throw is inside the bar" if not bad else "FAIL: %d problem(s)" % len(bad))
    return 0 if not bad else 1


if __name__ == "__main__":
    sys.exit(main())
