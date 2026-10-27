#!/usr/bin/env python3
"""Measures the hopper-hop-arc-g2 land stills and writes probe.json beside this script.

The gap this unit answers: the Hopper's hop arc settles it back over the trench lip and the ground under it stays
clean. The owner said it plainly -- "the landing has no dust; apex.png shows the walker in the air and the ground
under him is clean". So the question a still has to answer is only this: on the land frame, are there changed pixels
ON THE GROUND under the hull, more of them than the build without the dust ever showed? Nothing below is typed in:
every number comes out of the pixels and out of the .json sidecars CaptureRig wrote beside each still.

The stills come from Tests/Stills/HopperLandStills.cs, one locked-off camera (zoom 30, yaw 0, pitch 25) at night,
three landings of one run, each shot twice:
    land_N.png        the land frame, 3 pinned frames after the arc wrapped, the clock frozen
    land_N_empty.png  that same frozen instant with TankRenderer switched off: land_N's OWN plate
and the same six files from the build BEFORE the dust was added, kept here as base_N.png / base_N_empty.png.

Each mask is a still minus ITS OWN plate: one instant with the hull and without it, so what differs IS the Hopper and
whatever it put on the ground. A single plate taken at some other instant does not work -- the lamps, fog and men
move, and the topmost changed row then comes out as the sky.

    hull_top     the topmost image row whose mask holds at least ROW_MIN pixels. A single stray pixel is not a hull.
    hull_bottom  the last row of the hull block. The block ends where EMPTY_RUN rows in a row hold fewer than
                 ROW_MIN: the hull cruises 9 m up, so at zoom 30 a few HUNDRED empty rows separate it from the
                 ground. Taking the FIRST thin row instead does not work -- the legs and the waist thin to a few
                 pixels, and in one baseline still the block ended on the hull's own second row and 33 k pixels of
                 hull were counted as dust.
    dust_px      mask pixels in every row from hull_bottom + GAP down to the bottom of the frame. That is the ground
                 under the Hopper: the dust and the shadow disc, never the hull.

Two modes:
    --baseline   reads base_1..3 only, prints their dust_px, min, max and spread, writes baseline.json. Exit 0 when
                 all six base sidecars say zoom 30, pose_error_m < POSE_BAR, and luma_mean < NIGHT_BAR (night read
                 from the pixels, not typed in).
    (no flag)    reads land_1..3 and baseline.json. The bar is NOT a number chosen here: it is the top of the
                 baseline spread plus that spread again, so a pass cannot be run-to-run noise of the old build.
                 Exit 0 when every land_N beats that bar, and the land sidecars pass the same camera/night checks.
"""
import json
import os
import sys

import numpy as np
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))

DELTA = 30         # sum of the three channel differences a pixel must exceed to count as changed
ROW_MIN = 8        # mask pixels in a row before that row counts as part of a solid block
GAP = 4            # rows skipped below the hull block, so no hull edge leaks into the dust count
EMPTY_RUN = 24     # consecutive rows under ROW_MIN that end the hull block (a thin waist or a leg must not)
ZOOM = 30
POSE_BAR = 0.1
NIGHT_BAR = 0.35   # luma_mean of a night frame (the hopper-hop-arc stills came out at 0.17)
SPREAD_FACTOR = 1  # the bar sits one whole baseline spread above the baseline's top


def sidecar(stem):
    with open(os.path.join(HERE, stem + ".json"), encoding="utf-8") as f:
        return json.load(f)


def pixels(stem):
    return np.asarray(Image.open(os.path.join(HERE, stem + ".png")).convert("RGB"), dtype=np.int16)


def measure(stem):
    """The hull block and the changed pixels on the ground below it, for one still against its own plate."""
    mask = np.abs(pixels(stem) - pixels(stem + "_empty")).sum(axis=2) > DELTA
    per_row = mask.sum(axis=1)
    rows = np.nonzero(per_row >= ROW_MIN)[0]
    if rows.size == 0:
        return None
    top = int(rows[0])
    bottom, thin = top, 0
    for r in range(top + 1, per_row.size):
        if per_row[r] >= ROW_MIN:
            bottom, thin = r, 0
            continue
        thin += 1
        if thin >= EMPTY_RUN:
            break
    return {
        "hull_top": top,
        "hull_bottom": bottom,
        "dust_px": int(per_row[bottom + GAP:].sum()),
        "mask_px": int(mask.sum()),
    }


def cameras_ok(stems, out):
    """Every sidecar of these stills and their plates: the camera really was at zoom 30, reached the pose it was
    asked for, and the frame really was night."""
    cams = {}
    for s in stems:
        cams[s] = sidecar(s)
        cams[s + "_empty"] = sidecar(s + "_empty")
    out["zoom_in_sidecars"] = sorted({c.get("zoom") for c in cams.values()})
    out["pose_error_m"] = {k: round(c.get("pose_error_m", -1), 4) for k, c in cams.items()}
    out["luma_mean"] = {k: round(c.get("luma_mean", -1), 4) for k, c in cams.items()}
    zoom_ok = all(abs(c.get("zoom", -1) - ZOOM) < 0.01 for c in cams.values())
    pose_ok = all(c.get("pose_error_m", 9e9) < POSE_BAR for c in cams.values())
    night_ok = all(c.get("luma_mean", 9e9) < NIGHT_BAR for c in cams.values())
    if not zoom_ok:
        print("a sidecar was not shot at zoom %d: %s" % (ZOOM, out["zoom_in_sidecars"]), file=sys.stderr)
    if not pose_ok:
        print("the camera never reached a pose it was asked for: %s" % out["pose_error_m"], file=sys.stderr)
    if not night_ok:
        print("a still is not night (luma_mean >= %s): %s" % (NIGHT_BAR, out["luma_mean"]), file=sys.stderr)
    return zoom_ok and pose_ok and night_ok


def read(prefix):
    """measure() for prefix_1..3, or None with a message when a file or a hull is missing."""
    stems = ["%s_%d" % (prefix, n) for n in (1, 2, 3)]
    missing = [s + w for s in stems for w in (".png", "_empty.png", ".json", "_empty.json")
               if not os.path.exists(os.path.join(HERE, s + w))]
    if missing:
        print("missing files: " + ", ".join(missing), file=sys.stderr)
        return None, stems
    got = {s: measure(s) for s in stems}
    blind = [s for s, v in got.items() if v is None]
    if blind:
        print("no hull found against the plate in: " + ", ".join(blind), file=sys.stderr)
        return None, stems
    return got, stems


def save(name, data):
    with open(os.path.join(HERE, name), "w", encoding="utf-8") as f:
        json.dump(data, f, indent=2)
        f.write("\n")


def baseline():
    got, stems = read("base")
    if got is None:
        return 1
    vals = [got[s]["dust_px"] for s in stems]
    out = {
        "stills": got,
        "dust_px": vals,
        "min": min(vals),
        "max": max(vals),
        "spread": max(vals) - min(vals),
        "delta": DELTA,
        "row_min": ROW_MIN,
        "gap": GAP,
        "empty_run": EMPTY_RUN,
    }
    ok = cameras_ok(stems, out)
    save("baseline.json", out)
    for k in ("dust_px", "min", "max", "spread"):
        print(k, out[k])
    for s in stems:
        print(s, got[s])
    print("PASS" if ok else "FAIL")
    return 0 if ok else 1


def main():
    if "--baseline" in sys.argv[1:]:
        return baseline()
    path = os.path.join(HERE, "baseline.json")
    if not os.path.exists(path):
        print("no baseline.json: run probe.py --baseline on the base_* stills first", file=sys.stderr)
        return 1
    with open(path, encoding="utf-8") as f:
        base = json.load(f)
    got, stems = read("land")
    if got is None:
        return 1
    vals = [got[s]["dust_px"] for s in stems]
    bar = base["max"] + SPREAD_FACTOR * base["spread"]
    out = {
        "stills": got,
        "dust_px": vals,
        "min": min(vals),
        "max": max(vals),
        "spread": max(vals) - min(vals),
        "bar": bar,
        "baseline": {k: base[k] for k in ("dust_px", "min", "max", "spread")},
        "delta": DELTA,
        "row_min": ROW_MIN,
        "gap": GAP,
        "empty_run": EMPTY_RUN,
    }
    ok = cameras_ok(stems, out) and min(vals) > bar
    save("probe.json", out)
    for k in ("dust_px", "min", "max", "spread", "bar", "baseline"):
        print(k, out[k])
    if min(vals) <= bar:
        print("the dust on the ground is inside the old build's spread: %s against bar %s"
              % (vals, bar), file=sys.stderr)
    print("PASS" if ok else "FAIL")
    return 0 if ok else 1


if __name__ == "__main__":
    raise SystemExit(main())
