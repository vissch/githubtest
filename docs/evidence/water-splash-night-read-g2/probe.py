"""Measures the night water-death splash in the stills beside this file, and writes probe.json.

What is being proved. g1's probe (docs/evidence/water-splash-night-read/probe.py) measured one crystal: t1 and
t1-b are two renders of the SAME frozen frame, so noise_px is 0 by construction and "splash_px_t1 >= 6 x noise_px"
is a bar of 6 that any pixel of anything clears; o120's bar was "> 0", a bar of 1. Neither number could fail. This
stages THREE independent entries (AgentScripts/water_death_night_g2.cs) with live time running between them -
TWWater.hlsl drives foam and lapping from _Time.y - so each entry lands in a different phase of the water, and the
bar is set above what the water's OWN drift moves between two nosplash stills of different phases, not above zero.

The stills, one set of t1 (zoom 30) / o120 (zoom 120) pairs per entry i = 1, 2, 3:
  t1-<i>.png             the standing death, just after entry i's throw
  t1-<i>-nosplash.png    the same frozen instant with the splash lifted out (entry i's own reference)
  o120-<i>.png           the same instant, far band
  o120-<i>-nosplash.png  the far band's own reference
  t1-1.png / t1-1-b.png  one identical-frame repeat, kept for the record only (never a bar: see render_repeat_px)

A pixel counts as changed when its Rec.709 luma (0..1) moves by more than 0.02, the threshold water-entry-splash-r5
and g1 both used. The window is 3 m on mark_px_x/y and mark_px_per_m from each shot's own sidecar.

What it writes, per band (t1, o120):
  splash_px       three counts, entry i against its own nosplash reference
  drift_px        the three cross-entry nosplash-vs-nosplash pairs (1 vs 2, 2 vs 3, 1 vs 3): no splash is in
                  either side of any of these, so this is what the live water alone moves in the window between
                  two different phases - the honest noise floor g1's noise_px was never measuring
  bar             a literal picked after the staging run this file's comment names (see BAR_T1 / BAR_O120 below)

render_repeat_px is also written, t1-1 against t1-1-b: it is 0 by construction (two renders of one frozen frame),
which is exactly why it is reported for the record and never used as a bar.

Pass, per band: every one of the three splash_px at or above BAR, and BAR >= 3 * max(drift_px).

Run: python docs/evidence/water-splash-night-read-g2/probe.py
"""
import json
import os
import sys

from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
THRESHOLD = 0.02   # luma move that counts as a changed pixel
WINDOW_M = 3.0      # half-width and half-height of the window on the death point

# From the staging run on lane/show/water-splash-night-read-g2 (commit on top of e2301c9b77; stage.log of this
# run: three entries thrown at frames 89, 162, 229). For NEITHER band does a bar exist that is both at or below
# 0.7 * min(splash_px) and at or above 3 * max(drift_px): t1 needs <= 2065 and >= 9390; o120 needs <= 123 and
# >= 459. The water's own drift between phases is larger than 0.7 * the splash itself at this window and threshold.
# Per the leg card's risk note, the bar is set at the honest floor (3 * max(drift_px), rounded up to a multiple of
# ten) rather than lowered to make the file pass: this correctly fails both bands. See note.md for the finding.
BAR_T1 = 9390
BAR_O120 = 460


def luma(name):
    """Rec.709 luma, 0..1, as a row-major list read from the image's top-left."""
    im = Image.open(os.path.join(HERE, name)).convert("RGB")
    w, h = im.size
    px = im.load()
    out = [0.0] * (w * h)
    for y in range(h):
        row = y * w
        for x in range(w):
            r, g, b = px[x, y]
            out[row + x] = (0.2126 * r + 0.7152 * g + 0.0722 * b) / 255.0
    return out, w, h


def window(w, h, cx, cy, reach):
    idx = []
    for y in range(max(0, int(cy - reach)), min(h - 1, int(cy + reach)) + 1):
        row = y * w
        for x in range(max(0, int(cx - reach)), min(w - 1, int(cx + reach)) + 1):
            idx.append(row + x)
    return idx


def diff(a, b, idx):
    n = 0
    for i in idx:
        if abs(a[i] - b[i]) > THRESHOLD:
            n += 1
    return n


def count(name_a, name_b, side_name):
    side = json.load(open(os.path.join(HERE, side_name), encoding="utf-8"))
    cx, cy, ppm = side["mark_px_x"], side["mark_px_y"], side["mark_px_per_m"]
    a, w, h = luma(name_a)
    b, _, _ = luma(name_b)
    idx = window(w, h, cx, cy, WINDOW_M * ppm)
    return diff(a, b, idx)


def main():
    bands = {}
    for band in ("t1", "o120"):
        splash_px = [count(f"{band}-{i}.png", f"{band}-{i}-nosplash.png", f"{band}-{i}.json") for i in (1, 2, 3)]
        drift_px = [
            count(f"{band}-1-nosplash.png", f"{band}-2-nosplash.png", f"{band}-1.json"),
            count(f"{band}-2-nosplash.png", f"{band}-3-nosplash.png", f"{band}-2.json"),
            count(f"{band}-1-nosplash.png", f"{band}-3-nosplash.png", f"{band}-1.json"),
        ]
        bar = BAR_T1 if band == "t1" else BAR_O120
        bands[band] = {
            "splash_px": splash_px,
            "drift_px": drift_px,
            "bar": bar,
            "pass": all(s >= bar for s in splash_px) and bar >= 3 * max(drift_px),
        }

    render_repeat_px = count("t1-1.png", "t1-1-b.png", "t1-1.json")

    out = {
        "bands": bands,
        "render_repeat_px": render_repeat_px,
        "luma_threshold": THRESHOLD,
        "window_m": WINDOW_M,
    }
    ok = all(bands[b]["pass"] for b in bands)
    out["pass"] = ok
    with open(os.path.join(HERE, "probe.json"), "w", encoding="utf-8") as f:
        json.dump(out, f, indent=2)
        f.write("\n")
    print(json.dumps(out, indent=2))
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
