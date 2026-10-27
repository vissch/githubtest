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
                 baseline spread plus FOUR times that spread, so a pass cannot be run-to-run noise of the old
                 build; and on top of that the worst land still must hold twice the baseline's best, so a pass
                 means dust you can see, not a few hundred pixels more. Exit 0 when every land_N clears both,
                 the land sidecars pass the same camera/night checks, and all twelve cameras (both runs, stills
                 and plates) stand within CAM_BAR metres of each other -- which is also the proof that the two
                 runs found the SAME crossing, so the two sets are comparable at all.
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
SPREAD_FACTOR = 4  # the bar sits four whole baseline spreads above the baseline's top
DOUBLE = 2         # and the land stills must hold at least this many times the baseline's worst ground
CAM_BAR = 0.01     # metres the twelve cameras may differ by: the same locked-off pose, so the same crossing


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


def cameras_match(stems, out):
    """All twelve sidecars -- both runs, each still and its plate -- stood at one camera. If the second run had
    found a different crossing the rig would have aimed somewhere else, and the two sets would measure two
    different pieces of ground."""
    pos = {}
    for s in stems:
        c = sidecar(s)
        pos[s] = [round(float(c.get(k, -9e9)), 4) for k in ("cam_x", "cam_y", "cam_z")]
    out["cam_xyz"] = pos
    spans = [max(v[i] for v in pos.values()) - min(v[i] for v in pos.values()) for i in range(3)]
    out["cam_span_m"] = [round(v, 4) for v in spans]
    ok = all(v < CAM_BAR for v in spans)
    if not ok:
        print("the two runs did not stand at one camera (span %s m over %s): a different crossing was found"
              % (out["cam_span_m"], sorted(pos)), file=sys.stderr)
    return ok


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
    bar = max(base["max"] + SPREAD_FACTOR * base["spread"], DOUBLE * base["max"])
    out = {
        "stills": got,
        "dust_px": vals,
        "min": min(vals),
        "max": max(vals),
        "spread": max(vals) - min(vals),
        "bar": bar,
        "bar_spread": base["max"] + SPREAD_FACTOR * base["spread"],
        "bar_double": DOUBLE * base["max"],
        "baseline": {k: base[k] for k in ("dust_px", "min", "max", "spread")},
        "delta": DELTA,
        "row_min": ROW_MIN,
        "gap": GAP,
        "empty_run": EMPTY_RUN,
    }
    all_twelve = ["%s_%d%s" % (p, n, w) for p in ("base", "land") for n in (1, 2, 3) for w in ("", "_empty")]
    ok = cameras_ok(stems, out) and cameras_match(all_twelve, out) and min(vals) >= bar
    save("probe.json", out)
    for k in ("dust_px", "min", "max", "spread", "bar", "baseline"):
        print(k, out[k])
    if min(vals) < bar:
        print("the dust on the ground does not clear the bar: %s against bar %s"
              % (vals, bar), file=sys.stderr)
    print("PASS" if ok else "FAIL")
    return 0 if ok else 1


if __name__ == "__main__":
    raise SystemExit(main())
