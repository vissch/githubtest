#!/usr/bin/env python3
"""Measures the hopper-hop-arc stills and writes probe.json beside this script.

The gap this unit answers: the Hopper cruised dead level at its 9 m lift over every trench and wire belt. The arc in
TankRenderer.HopArc.cs lifts it further and settles it back. The question a still has to answer is only this: does
the hull actually move UP the screen, by enough that a person sees it? Nothing below is typed in; every number comes
out of the pixels and out of the .json sidecars CaptureRig wrote beside each still.

The stills come from Tests/Stills/HopperHopStills.cs, one locked-off camera (zoom 30, yaw 0, pitch 25) shot four
times over a trench found in the map, not chosen by hand:
    cruise.png    the Hopper held over the trench at arc phase 0, i.e. its plain cruise, the clock frozen
    empty.png     that same frozen instant with the renderer switched off: the cruise still's plate
    apex.png      the top of one arc (phase 0.5), the clock frozen
    apex_b.png    the SAME frozen instant photographed again: the noise floor
    empty_apex.png  that frozen instant with the renderer switched off: the plate for both apex stills

Each hull mask is a still minus ITS OWN plate: one instant with the hull and without it, so what differs IS the
Hopper (and the disc and dust it puts on the ground). A single plate taken at some other instant does not work --
the lamps, fog and men move, and the topmost changed row then comes out as the sky.

    top_px(stem)  the topmost image row whose mask holds at least ROW_MIN pixels. A single stray pixel is not a
                  hull, so a row has to carry a hull's worth of them before it counts as the top of one.
    apex_px       top_px(cruise) - top_px(apex): how far the hull rose on screen between its cruise and the arc's
                  top. Positive means up (image rows count down from the top). Bar 20.
    floor_px      |top_px(apex) - top_px(apex_b)|: the same pose twice, so whatever this is, the rig cannot repeat
                  it. apex_px has to beat it four times over.

Exit 0 only when apex_px >= APEX_BAR and apex_px >= 4 * floor_px, and every sidecar says the camera really was at
zoom 30 and reached the pose it was asked for (pose_error_m < 0.1 m).
"""
import json
import os
import sys

import numpy as np
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))

DELTA = 30         # sum of the three channel differences a pixel must exceed to count as changed
ROW_MIN = 8        # mask pixels in a row before that row counts as the top of something
APEX_BAR = 20
FLOOR_FACTOR = 4
ZOOM = 30
POSE_BAR = 0.1

PLATE = {"cruise": "empty", "apex": "empty_apex", "apex_b": "empty_apex"}
STEMS = tuple(PLATE)


def sidecar(stem):
    with open(os.path.join(HERE, stem + ".json"), encoding="utf-8") as f:
        return json.load(f)


def pixels(stem):
    return np.asarray(Image.open(os.path.join(HERE, stem + ".png")).convert("RGB"), dtype=np.int16)


def top_px(stem):
    """The topmost row of this still's mask against the plate, and how many pixels the whole mask holds."""
    mask = np.abs(pixels(stem) - pixels(PLATE[stem])).sum(axis=2) > DELTA
    per_row = mask.sum(axis=1)
    rows = np.nonzero(per_row >= ROW_MIN)[0]
    if rows.size == 0:
        return None, int(mask.sum())
    return int(rows[0]), int(mask.sum())


def main():
    missing = [s for s in set(STEMS) | set(PLATE.values()) if not os.path.exists(os.path.join(HERE, s + ".png"))]
    if missing:
        print("missing stills: " + ", ".join(sorted(missing)), file=sys.stderr)
        return 1

    tops, areas = {}, {}
    for s in STEMS:
        tops[s], areas[s] = top_px(s)
    if any(v is None for v in tops.values()):
        print("no hull found against the plate in: "
              + ", ".join(s for s, v in tops.items() if v is None), file=sys.stderr)
        return 1

    cams = {s: sidecar(s) for s in sorted(set(STEMS) | set(PLATE.values()))}
    zoom_ok = all(abs(c.get("zoom", -1) - ZOOM) < 0.01 for c in cams.values())
    pose_ok = all(c.get("pose_error_m", 9e9) < POSE_BAR for c in cams.values())

    apex_px = tops["cruise"] - tops["apex"]
    floor_px = abs(tops["apex"] - tops["apex_b"])

    result = {
        "apex_px": apex_px,
        "floor_px": floor_px,
        "zoom": ZOOM,
        "top_px": tops,
        "mask_px": areas,
        "pose_error_m": {s: round(c.get("pose_error_m", -1), 4) for s, c in cams.items()},
        "zoom_in_sidecars": sorted({c.get("zoom") for c in cams.values()}),
        "delta": DELTA,
        "row_min": ROW_MIN,
    }
    with open(os.path.join(HERE, "probe.json"), "w", encoding="utf-8") as f:
        json.dump(result, f, indent=2)
        f.write("\n")

    for k in ("apex_px", "floor_px", "top_px", "mask_px"):
        print(k, result[k])
    ok = apex_px >= APEX_BAR and apex_px >= FLOOR_FACTOR * floor_px and zoom_ok and pose_ok
    if not zoom_ok:
        print("a sidecar was not shot at zoom %d: %s" % (ZOOM, result["zoom_in_sidecars"]), file=sys.stderr)
    if not pose_ok:
        print("the camera never reached a pose it was asked for: %s" % result["pose_error_m"], file=sys.stderr)
    print("PASS" if ok else "FAIL")
    return 0 if ok else 1


if __name__ == "__main__":
    raise SystemExit(main())
