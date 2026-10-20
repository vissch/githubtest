#!/usr/bin/env python3
"""Picks where the prone AT rifleman is staged, on measurement and not on taste.

The question this answers: at zoom 30, yaw 180, WHERE does the prone AT rifleman stop being a dark
lump in the sandbag gap? Behind the parapet his mask was 286-383 px against the kneeling Sniper's
1327-1633 px (docs/evidence/atrifle-look-r3-g1), at-2 looked empty, and round 1 left 1057 weather
pixels inside his box. The camera at yaw 180 sits at high z looking toward -z, so the parapet at z
about 26 stands between it and a man at z 25: the twelve candidates are x 49/50/51 by z 27/28/29/30,
all on the camera's side of the bags. Four stills each, the same camera as the whole set.

Nothing in the game is changed by this: the man's place is staging. The bags, the sim and the clips
are untouched.

THE THREE HARD TESTS are the owner's words turned into numbers, and they are not negotiable here:
  * his mask is at least HALF the kneeling man's mask, in pixels. That is "a player can see him";
  * ZERO weather pixels inside his box. |empty - floor| is two man-less plates of the same frame a
    few world-seconds apart, so everything in it moves on its own; round 1 of the previous set had
    1057 of them in his box;
  * the two boxes DO NOT OVERLAP, so a mask holds one man and not two.
Plus the frame's own bookkeeping: men_in_frame 1 / 1 / 0 / 0 over at / sniper / empty / floor,
drawn_infantry 0 in both plates, one camera for all four stills, pose_error_m 0, rain 0, no warning,
and a largest blob that earns the name "a man" by mask.py's size and dominance rules.

AMONG THE CANDIDATES THAT PASS, the one kept is the one whose drawn heading is closest to the
Sniper's. A man's yaw cannot be written; he is turned by being made to step
(AnimationController.cs:462), so the staging ends with a one-metre hop along the Sniper's own drawn
yaw. That is what replaces the fixed 1.11 rad of the previous set, where part of every reading was
heading and not build. Each candidate's drawn heading is read back out of the sim: cand-poses.json.

Exit 0 and staging.json written when a candidate clears everything; exit non-zero otherwise, with
every candidate's numbers printed either way.
"""
import json
import math
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

import mask

HERE = os.path.dirname(os.path.abspath(__file__))
KINDS = ("at", "sniper", "empty", "floor")
MEN = {"at": 1, "sniper": 1, "empty": 0, "floor": 0}
FRAME_KEYS = ("zoom", "yaw", "pitch", "focus_x", "focus_z")
MIN_MASK_RATIO = 0.5        # of the kneeling man's mask: the owner's "half the kneel's"
MAX_NOISE_IN_BOX = 0        # weather pixels inside his box


def wrap(a):
    return abs((a + math.pi) % (2 * math.pi) - math.pi)


def judge_blob(r, lo, who, bad):
    """mask.py found the largest blob in his box; does it earn the name "a man"?"""
    if r.get("why"):
        bad.append("%s: %s" % (who, r["why"]))
        return False
    ok = True
    hi = int(round(mask.MAX_BOX_SHARE * r["box_px"]))
    if r["mask_px"] < lo:
        bad.append("%s: %d px is under the frame's floor of %d px (%s m2)"
                   % (who, r["mask_px"], lo, mask.MIN_AREA_M2))
        ok = False
    if r["mask_px"] > hi:
        bad.append("%s: %d px is over %d px, more than %.0f%% of his own box"
                   % (who, r["mask_px"], hi, 100 * mask.MAX_BOX_SHARE))
        ok = False
    if r["second_px"] * mask.DOMINANCE > r["mask_px"]:
        bad.append("%s: largest %d px against the next %d px, under %sx - ambiguous"
                   % (who, r["mask_px"], r["second_px"], mask.DOMINANCE))
        ok = False
    for side, cut in r["cuts"].items():
        if cut:
            bad.append("%s: his box runs off the %s frame edge" % (who, side))
            ok = False
    return ok


def candidate(n, poses, shape):
    stems = {k: "cand%d-%s" % (n, k) for k in KINDS}
    bad = []
    row = {"n": n, "pos": poses["cand%d" % n]["pos"]}
    for k, s in stems.items():
        if not (os.path.exists(os.path.join(HERE, s + ".png"))
                and os.path.exists(os.path.join(HERE, s + ".json"))):
            return {"n": n, "problems": ["cand%d: %s is missing" % (n, s)], "passes": False}

    cam = None
    for k, s in stems.items():
        d = mask.sidecar(HERE, s)
        got = {key: float(d[key]) for key in FRAME_KEYS}
        if cam is None:
            cam = got
            row["camera"] = got
        elif got != cam:
            bad.append("%s: camera %s is not this candidate's %s" % (s, got, cam))
        if float(d["pose_error_m"]) != 0:
            bad.append("%s: pose_error_m %s, want 0" % (s, d["pose_error_m"]))
        if float(d["rain"]) != 0:
            bad.append("%s: rain %s, want 0" % (s, d["rain"]))
        if d["warnings"]:
            bad.append("%s: the capture warns %s" % (s, d["warnings"]))
        if int(d["men_in_frame"]) != MEN[k]:
            bad.append("%s: men_in_frame %s, want %d" % (s, d["men_in_frame"], MEN[k]))
        if MEN[k] == 0 and int(d.get("drawn_infantry", 0)) != 0:
            bad.append("%s: a plate with no man in frame drew %s - that is not a plate"
                       % (s, d["drawn_infantry"]))

    noise = mask.raw_diff(HERE, stems["empty"], stems["floor"])
    row["noise_px"] = int(noise.sum())
    men = {}
    for k in ("at", "sniper"):
        men[k] = mask.man_in_box(HERE, stems[k], stems["empty"], noise, shape)
        row["box_" + k] = men[k].get("box")
        row["mask_px_" + k] = men[k].get("mask_px")
        row["second_px_" + k] = men[k].get("second_px")
        row["noise_in_box_px_" + k] = men[k].get("noise_in_box_px")
        row["px_per_m_" + k] = round(men[k]["px_per_m"], 3) if "px_per_m" in men[k] else None

    scale = men["sniper"].get("px_per_m") or men["at"].get("px_per_m")
    lo = int(round(mask.MIN_AREA_M2 * scale ** 2)) if scale else 0
    row["mask_px_min"] = lo
    kept = all(judge_blob(men[k], lo, "cand%d-%s" % (n, k), bad) for k in ("at", "sniper"))

    # --- the three hard tests -------------------------------------------------------------------
    if men["at"].get("mask_px") and men["sniper"].get("mask_px"):
        ratio = men["at"]["mask_px"] / float(men["sniper"]["mask_px"])
        row["mask_ratio"] = round(ratio, 4)
        if ratio < MIN_MASK_RATIO:
            bad.append("cand%d: his mask is %d px against the kneel's %d px, ratio %.3f, under %s "
                       "- a player still cannot see him"
                       % (n, men["at"]["mask_px"], men["sniper"]["mask_px"], ratio, MIN_MASK_RATIO))
    else:
        row["mask_ratio"] = None
    if (men["at"].get("noise_in_box_px") or 0) > MAX_NOISE_IN_BOX:
        bad.append("cand%d: %d weather pixels fall inside his box, want %d"
                   % (n, men["at"]["noise_in_box_px"], MAX_NOISE_IN_BOX))
    if (men["sniper"].get("noise_in_box_px") or 0) > MAX_NOISE_IN_BOX:
        bad.append("cand%d: %d weather pixels fall inside the kneel's box, want %d"
                   % (n, men["sniper"]["noise_in_box_px"], MAX_NOISE_IN_BOX))
    ba, bs = men["at"].get("box"), men["sniper"].get("box")
    if ba and bs:
        over = not (ba[1] <= bs[0] or bs[1] <= ba[0] or ba[3] <= bs[2] or bs[3] <= ba[2])
        row["boxes_overlap"] = bool(over)
        if over:
            bad.append("cand%d: the boxes %s and %s overlap - a mask would hold both men"
                       % (n, ba, bs))

    p = poses["cand%d" % n]
    row["at_yaw"] = p["at_yaw"]
    row["sniper_yaw"] = p["sniper_yaw"]
    row["at_clip"] = p["at_clip"]
    row["sniper_clip"] = p["sniper_clip"]
    row["heading_diff_rad"] = round(wrap(p["at_yaw"] - p["sniper_yaw"]), 4)
    if p["at_clip"] != "ProneIdle":
        bad.append("cand%d: the AT rifleman is in %s, not ProneIdle" % (n, p["at_clip"]))
    if p["sniper_clip"] != "KneelIdle":
        bad.append("cand%d: the Sniper is in %s, not KneelIdle" % (n, p["sniper_clip"]))

    row["problems"] = bad
    row["passes"] = bool(kept and not bad)
    return row


def main():
    with open(os.path.join(HERE, "cand-poses.json"), encoding="utf-8") as f:
        poses = json.load(f)
    ns = sorted(int(k[4:]) for k in poses if k.startswith("cand") and k[4:].isdigit())
    shape = (900, 1600)
    rows = [candidate(n, poses, shape) for n in ns]
    ok = [r for r in rows if r.get("passes")]
    ok.sort(key=lambda r: (r["heading_diff_rad"], -r["mask_ratio"]))
    report = {"min_mask_ratio": MIN_MASK_RATIO, "max_noise_in_box_px": MAX_NOISE_IN_BOX,
              "candidates": rows, "passed": [r["n"] for r in ok]}
    if ok:
        k = ok[0]
        report["kept"] = {"n": k["n"], "pos": k["pos"], "mask_ratio": k["mask_ratio"],
                          "mask_px_at": k["mask_px_at"], "mask_px_sniper": k["mask_px_sniper"],
                          "noise_in_box_px_at": k["noise_in_box_px_at"],
                          "heading_diff_rad": k["heading_diff_rad"], "box_at": k["box_at"],
                          "box_sniper": k["box_sniper"], "sniper_yaw": k["sniper_yaw"]}
    with open(os.path.join(HERE, "staging.json"), "w", encoding="utf-8") as f:
        json.dump(report, f, indent=2)
        f.write("\n")
    print(json.dumps(report, indent=2))
    if not ok:
        print("FAIL: no candidate clears the three hard tests", file=sys.stderr)
        return 2
    return 0


if __name__ == "__main__":
    sys.exit(main())
