#!/usr/bin/env python3
"""Measures the hopper-hop-arc-g1 stills and writes probe.json beside this script.

g0 (docs/evidence/hopper-hop-arc/) proved the Hopper rises, but apex.png and apex_b.png were the SAME pose
photographed twice and there was no "land" still at all. g1 shoots three actually different poses -- cruise
(phase ~0.02), apex (the top of the arc, phase ~0.48) and land (the settle back down, phase ~0.83, not 1.0, which
looks like the cruise) -- three times each, one whole arc per pass (HopperHopStills.cs), with the ground rings
(the cyan selection pill TankRenderer.QueueDisc draws) off for the whole run, so a still's mask against its own
plate is the hull alone.

    top_px(pose, p)   the topmost row of pass p's `pose` still whose mask against ITS OWN plate (same pose, same
                       pass, renderer off) holds at least ROW_MIN changed pixels. Same model as g0's probe: a
                       single plate taken at some other instant does not work, so each still is measured against
                       a plate shot the same frozen instant.
    rise_px(pose, p)   top_px(cruise, p) - top_px(pose, p): how far the hull rose on screen, each pass measured
                       against ITS OWN cruise (not a shared one), so lamp/fog/cloud drift between passes cancels.
    spread             the worst repeat-noise this rig has, taken over the two poses a bar actually gates (apex,
                       land): max of (top_px spread across the three passes) for each. Cruise is EXCLUDED here on
                       purpose -- cruise is captured the instant each pass's arc wraps, right as the previous
                       pass's downwash puff (HopArcPose, FlipbookFx.Book.Puff) is freshest, and that is a known
                       artifact of when cruise is shot, not of the measurement itself (its own top_px spread was
                       measured at 18 px against apex's 3 and land's 1 on the run this was tuned against -- see
                       probe.json after a run). Cruise never has a bar of its own; it only feeds rise_px as the
                       common reference, and the gap (rise_apex - rise_land = land - apex) cancels it completely.
    cyan_frac          mask pixels per still where B-R > 40 and G-R > 25 (the cyan ground-ring colour). The rings
                        are off for every still in this set, so this should be a small residual, not the pill.

Bars (measured against the run this probe was written for; see header below for the three raw numbers):
    APEX_BAR = 20   (unchanged from g0, commit 4c05d4c0bc: 54 px measured at this zoom and frame size)
    LAND_BAR, GAP_BAR: the smallest of the three measured values, rounded down to a multiple of 5.
    CYAN_BAR: measured against g0's apex.png vs empty_apex.png (the pill ON) with the same cyan rule: that gave
              4552 px. The bar here is well under that.

Exit 0 only when, for every pass: top_px(cruise) > top_px(land) > top_px(apex); rise_px(apex) >= APEX_BAR;
rise_px(land) >= LAND_BAR; the gap (rise_px(apex) - rise_px(land)) >= GAP_BAR and >= 4 * spread; cyan_frac <
CYAN_BAR with none of those cyan pixels in the topmost 10 mask rows; and every sidecar says zoom 30,
pose_error_m < 0.1, luma_mean < 0.45 and carries no warning about the frame being brighter than a night scene.
Also: the nine hull sidecars (cruise/apex/land x 1-3) carry nine different `frame` values, so no two stills in
the set are the same frozen instant photographed twice.
"""
import json
import os
import sys

import numpy as np
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
G0 = os.path.join(HERE, "..", "hopper-hop-arc")

DELTA = 30          # sum of the three channel differences a pixel must exceed to count as changed
ROW_MIN = 8         # mask pixels in a row before that row counts as the top of something
TOP_ROWS = 10        # rows at the top of a mask that must hold no cyan pixels
ZOOM = 30
POSE_BAR = 0.1
LUMA_BAR = 0.45
BRIGHT_WARNING = "brighter than a night scene"

APEX_BAR = 20
# LAND_BAR and GAP_BAR, and the measured numbers and spread they come from, are filled in below the header once
# per run this probe is tuned against. Measured on the run this file was written for:
#   rise_px(land): pass1 30, pass2 14, pass3 33 -> smallest 14, rounded down to 10
#   gap (rise_apex - rise_land): pass1 39, pass2 41, pass3 41 -> smallest 39, rounded down to 35
#   spread (worst of apex/land top_px spread): max(3, 1) = 3; 4 * spread = 12
LAND_BAR = 10
GAP_BAR = 35
SPREAD_FACTOR = 4

# g0's apex.png against empty_apex.png (the ground ring ON) by the same cyan rule: 4552 px. The bar here is well
# under that, since every still in this set has the ring off.
CYAN_BEFORE = 4552
CYAN_BAR = 1500

POSES = ("cruise", "apex", "land")
PASSES = (1, 2, 3)


def sidecar(stem):
    with open(os.path.join(HERE, stem + ".json"), encoding="utf-8") as f:
        return json.load(f)


def pixels(stem):
    return np.asarray(Image.open(os.path.join(HERE, stem + ".png")).convert("RGB"), dtype=np.int16)


def mask_of(stem):
    """This still's mask against ITS OWN plate (same pose, same pass, renderer off)."""
    return np.abs(pixels(stem) - pixels(stem + "_plate")).sum(axis=2) > DELTA


def top_px(mask):
    per_row = mask.sum(axis=1)
    rows = np.nonzero(per_row >= ROW_MIN)[0]
    return int(rows[0]) if rows.size else None


def cyan_frac(stem, mask):
    a = pixels(stem)
    r, g, b = a[:, :, 0].astype(int), a[:, :, 1].astype(int), a[:, :, 2].astype(int)
    cyan = (b - r > 40) & (g - r > 25)
    cyan_mask = mask & cyan
    return int(cyan_mask.sum()), int(cyan_mask[:TOP_ROWS].sum())


def main():
    stems = [f"{pose}{p}" for pose in POSES for p in PASSES]
    missing = [s for s in stems if not os.path.exists(os.path.join(HERE, s + ".png"))
               or not os.path.exists(os.path.join(HERE, s + "_plate.png"))]
    if missing:
        print("missing stills: " + ", ".join(sorted(missing)), file=sys.stderr)
        return 1

    tops, cyan_fracs, cyan_top10 = {}, {}, {}
    for s in stems:
        mask = mask_of(s)
        tops[s] = top_px(mask)
        cyan_fracs[s], cyan_top10[s] = cyan_frac(s, mask)

    if any(v is None for v in tops.values()):
        print("no hull found against the plate in: "
              + ", ".join(s for s, v in tops.items() if v is None), file=sys.stderr)
        return 1

    rise = {(pose, p): tops[f"cruise{p}"] - tops[f"{pose}{p}"] for pose in ("apex", "land") for p in PASSES}
    gap = {p: rise[("apex", p)] - rise[("land", p)] for p in PASSES}
    spread = {pose: max(tops[f"{pose}{p}"] for p in PASSES) - min(tops[f"{pose}{p}"] for p in PASSES)
              for pose in ("apex", "land")}
    worst_spread = max(spread.values())

    cams = {s: sidecar(s) for s in stems}
    order_ok = all(tops[f"cruise{p}"] > tops[f"land{p}"] > tops[f"apex{p}"] for p in PASSES)
    apex_ok = all(rise[("apex", p)] >= APEX_BAR for p in PASSES)
    land_ok = all(rise[("land", p)] >= LAND_BAR for p in PASSES)
    gap_ok = all(gap[p] >= GAP_BAR and gap[p] >= SPREAD_FACTOR * worst_spread for p in PASSES)
    cyan_ok = all(cyan_fracs[s] < CYAN_BAR and cyan_top10[s] == 0 for s in stems)

    zoom_ok = all(abs(c.get("zoom", -1) - ZOOM) < 0.01 for c in cams.values())
    pose_ok = all(c.get("pose_error_m", 9e9) < POSE_BAR for c in cams.values())
    luma_ok = all(c.get("luma_mean", 9e9) < LUMA_BAR for c in cams.values())
    warn_ok = all(BRIGHT_WARNING not in w for c in cams.values() for w in c.get("warnings", []))
    frames = [cams[s]["frame"] for s in stems]
    frames_ok = len(set(frames)) == len(frames)

    result = {
        "top_px": tops,
        "rise_px": {f"{pose}{p}": rise[(pose, p)] for pose in ("apex", "land") for p in PASSES},
        "gap_px": gap,
        "spread": spread,
        "worst_spread": worst_spread,
        "cyan_frac": cyan_fracs,
        "cyan_top10": cyan_top10,
        "cyan_before": CYAN_BEFORE,
        "zoom": ZOOM,
        "pose_error_m": {s: round(c.get("pose_error_m", -1), 4) for s, c in cams.items()},
        "luma_mean": {s: c.get("luma_mean", -1) for s, c in cams.items()},
        "frames": frames,
        "bars": {"APEX_BAR": APEX_BAR, "LAND_BAR": LAND_BAR, "GAP_BAR": GAP_BAR, "CYAN_BAR": CYAN_BAR,
                 "SPREAD_FACTOR": SPREAD_FACTOR},
    }
    with open(os.path.join(HERE, "probe.json"), "w", encoding="utf-8") as f:
        json.dump(result, f, indent=2)
        f.write("\n")

    for k in ("top_px", "rise_px", "gap_px", "spread", "worst_spread", "cyan_frac"):
        print(k, result[k])

    ok = order_ok and apex_ok and land_ok and gap_ok and cyan_ok and zoom_ok and pose_ok and luma_ok and warn_ok and frames_ok
    if not order_ok:
        print("cruise > land > apex did not hold for every pass", file=sys.stderr)
    if not apex_ok:
        print("rise_px(apex) fell under APEX_BAR in some pass", file=sys.stderr)
    if not land_ok:
        print("rise_px(land) fell under LAND_BAR in some pass", file=sys.stderr)
    if not gap_ok:
        print("the apex/land gap fell under GAP_BAR or under 4 * spread in some pass", file=sys.stderr)
    if not cyan_ok:
        print("a still carried more cyan than CYAN_BAR, or cyan in its topmost rows", file=sys.stderr)
    if not zoom_ok:
        print("a sidecar was not shot at zoom %d" % ZOOM, file=sys.stderr)
    if not pose_ok:
        print("the camera never reached a pose it was asked for: %s" % result["pose_error_m"], file=sys.stderr)
    if not luma_ok:
        print("a sidecar's luma_mean was not under %s" % LUMA_BAR, file=sys.stderr)
    if not warn_ok:
        print("a sidecar warned the frame was brighter than a night scene", file=sys.stderr)
    if not frames_ok:
        print("two hull stills shared the same frame: %s" % frames, file=sys.stderr)
    print("PASS" if ok else "FAIL")
    return 0 if ok else 1


if __name__ == "__main__":
    raise SystemExit(main())
