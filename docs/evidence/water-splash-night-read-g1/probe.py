"""Measures three night water-death splashes in the stills beside this file, and writes probe.json.

What is being proved. The night death staged by water-splash-night-read read as ONE pale blue crystal in the
creek: no crown, no wings, no foam skirt, no body. WaterEntry plans eight cards, but it draws all eight on
Book.Splash - the same upright water-white column sheet - inside one 3 m footprint, so they stack into a single
pale blob. The lobes exist in the arithmetic and not on screen. This probe reads the lobes directly: it counts the
changed pixels in a ring 1.5 m to 3.0 m out from the death point, where the wings, droplets and skirt must land.

The stills. AgentScripts/water_death_night_g1.cs stages three deaths about 2.5 m apart along the GreyboxCorridor
ford, in Play at night (PinnedClock 2 AM, SceneMood.Night = true), with the two production calls of a standing
death in water (units.AddFallen then WaterEntry.Throw) and two live men held still in the frame for scale.
CaptureRig.Mark names each death point before its triple, so each sidecar carries that point's projection
(mark_px_x/y, mark_px_per_m). Each death gets three renders of the same frozen frame:

  <name>.png            the death
  <name>-b.png          the identical frozen frame rendered a second time (the noise floor)
  <name>-nosplash.png   the identical frozen frame with the splash lifted out (rings parked, card list emptied)

A pixel counts as changed when its Rec.709 luma (0..1) moves by more than 0.02, the threshold
water-entry-splash-r5 used.

Per capture it writes:
  core_px        changed pixels within 0.75 m of the mark (the blob that is already there)
  spread_px      changed pixels in the annulus 1.5 m to 3.0 m (the lobes the owner says are missing)
  reach_m        the 95th-percentile distance, in metres, of a changed pixel from the mark
  noise_px       the capture against its own -b render
  men_in_frame   read straight out of the capture's sidecar json

Two modes.
  probe.py before   reads before-a/b/c, writes before.json (three readings and their min/max), always exit 0.
                    A record of the unchanged build, no bar.
  probe.py          reads a1/a2/a3, writes probe.json, exit 0 only when ALL THREE clear the bar.

The bar. SPREAD_BAR is twice the widest spread_px of the before run on e2301c9b77 (the unchanged build, this
probe's own stage script), rounded up to a round number: the three before readings were 1190, 1211 and 1017 px,
so 2 x 1211 = 2422 -> 2500. before.json holds that run. Outside the spread of three captures by a wide margin:
the three sat within 200 px of each other and 0 noise px, so the bar is not within reach of run-to-run wobble.
REACH_BAR is a floor, not the discriminator: the shader's water ring already opens to 2.8 m, so the before build
reads reach_m 2.92 - the bar only catches a splash that collapses into the blob. The discriminator is spread_px.

Run: python docs/evidence/water-splash-night-read-g1/probe.py
"""
import json
import math
import os
import sys

from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
THRESHOLD = 0.02    # luma move that counts as a changed pixel
WINDOW_M = 3.0      # half-width and half-height of the window on the death point
CORE_M = 0.75       # the blob radius
RING_IN_M = 1.5     # the lobe band, inner
RING_OUT_M = 3.0    # the lobe band, outer

SPREAD_BAR = 2500   # twice the widest before reading (1211 px), rounded up; see the docstring
REACH_BAR = 2.0     # metres, 95th-percentile distance of a changed pixel
NOISE_FRAC = 0.02   # noise_px must be no more than this share of core_px

SETS = {
    "before": ["before-a", "before-b", "before-c"],
    "after": ["a1", "a2", "a3"],
}


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


def changed(a, b, w, h, cx, cy, reach):
    """Distances in pixels from (cx, cy) of every changed pixel inside the square window."""
    out = []
    for y in range(max(0, int(cy - reach)), min(h - 1, int(cy + reach)) + 1):
        row = y * w
        dy = y - cy
        for x in range(max(0, int(cx - reach)), min(w - 1, int(cx + reach)) + 1):
            if abs(a[row + x] - b[row + x]) > THRESHOLD:
                dx = x - cx
                out.append(math.sqrt(dx * dx + dy * dy))
    return out


def read(name):
    side = json.load(open(os.path.join(HERE, name + ".json"), encoding="utf-8"))
    cx, cy, ppm = side["mark_px_x"], side["mark_px_y"], side["mark_px_per_m"]
    shot, w, h = luma(name + ".png")
    ref, _, _ = luma(name + "-nosplash.png")
    again, _, _ = luma(name + "-b.png")

    reach = WINDOW_M * ppm
    d = changed(shot, ref, w, h, cx, cy, reach)
    core = sum(1 for r in d if r <= CORE_M * ppm)
    spread = sum(1 for r in d if RING_IN_M * ppm < r <= RING_OUT_M * ppm)
    d.sort()
    reach_m = (d[int(0.95 * (len(d) - 1))] / ppm) if d else 0.0
    noise = len(changed(shot, again, w, h, cx, cy, reach))
    return {
        "name": name,
        "core_px": core,
        "spread_px": spread,
        "reach_m": round(reach_m, 3),
        "noise_px": noise,
        "men_in_frame": side.get("men_in_frame", 0),
        "changed_px": len(d),
    }


def judge(r):
    why = []
    if r["spread_px"] < SPREAD_BAR:
        why.append("spread_px %d < %d" % (r["spread_px"], SPREAD_BAR))
    if r["reach_m"] < REACH_BAR:
        why.append("reach_m %.3f < %.1f" % (r["reach_m"], REACH_BAR))
    if r["men_in_frame"] < 1:
        why.append("men_in_frame 0: no man to read the splash against")
    if r["noise_px"] > NOISE_FRAC * r["core_px"]:
        why.append("noise_px %d > %.1f (%.0f%% of core_px %d)"
                   % (r["noise_px"], NOISE_FRAC * r["core_px"], 100 * NOISE_FRAC, r["core_px"]))
    return why


def main():
    before = len(sys.argv) > 1 and sys.argv[1] == "before"
    names = SETS["before" if before else "after"]
    missing = [n for n in names if not os.path.exists(os.path.join(HERE, n + ".png"))]
    if missing:
        print("missing captures: " + ", ".join(missing))
        return 0 if before else 1

    rows = [read(n) for n in names]
    out = {
        "captures": rows,
        "luma_threshold": THRESHOLD,
        "core_m": CORE_M,
        "ring_m": [RING_IN_M, RING_OUT_M],
        "window_m": WINDOW_M,
    }
    for key in ("core_px", "spread_px", "reach_m", "noise_px"):
        out["min_" + key] = min(r[key] for r in rows)
        out["max_" + key] = max(r[key] for r in rows)

    if before:
        out["note"] = "a record of the unchanged build; no bar is applied"
        path = "before.json"
        ok = True
    else:
        out["spread_bar"] = SPREAD_BAR
        out["reach_bar"] = REACH_BAR
        fails = {r["name"]: judge(r) for r in rows}
        out["failed"] = {k: v for k, v in fails.items() if v}
        ok = not out["failed"]
        out["pass"] = ok
        path = "probe.json"

    with open(os.path.join(HERE, path), "w", encoding="utf-8") as f:
        json.dump(out, f, indent=2)
        f.write("\n")
    print(json.dumps(out, indent=2))
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
