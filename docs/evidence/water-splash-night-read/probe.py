"""Measures the night water-death splash in the stills beside this file, and writes probe.json.

What is being proved. Harsh critic r1 (38/100) fix 1: at night, six staged 80 kg entries into the GreyboxCorridor
ford read as 292 px against 118 px of noise (2.5x) - a few faint blue streaks, not a man dying knee-deep. This
counts the pixels of a real standing death at night, same trick as water-entry-splash-r5's probe.py: each moment
gets its OWN reference, the identical frozen frame re-rendered with the splash lifted out (WaterRings' rings
parked and FlipbookFx's card list emptied), so the difference between a moment and its reference is the water
entry and nothing else.

The stills. AgentScripts/water_death_night.cs staged them and CaptureRig rendered them, in Play in GreyboxCorridor
at night (PinnedClock 2 AM, SceneMood.Night = true), at the ford point (15, 130) CaptureRig.Mark named before the
shots, so every sidecar carries that point's projection (mark_px_x/y and mark_px_per_m).

  t1.png             T1 (zoom 30), the standing death, just after the throw
  t1-b.png           the same frozen frame rendered a second time (the noise floor)
  t1-nosplash.png    the same frozen frame with the splash lifted out
  o120.png           O120 (zoom 120), the same moment, far band
  o120-nosplash.png  the same frozen O120 frame with the splash lifted out

A pixel counts as changed when its Rec.709 luma (0..1) moves by more than 0.02, the same threshold
water-entry-splash-r5 used.

What it writes:
  splash_px_t1    changed pixels between t1 and t1-nosplash, in a window on the mark
  noise_px        changed pixels between t1 and t1-b (two renders of the identical frozen frame)
  splash_px_o120  changed pixels between o120 and o120-nosplash, in a window on the mark

Pass: splash_px_t1 >= 6 x noise_px, splash_px_o120 > 0.

Run: python docs/evidence/water-splash-night-read/probe.py
"""
import json
import os
import sys

from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
THRESHOLD = 0.02   # luma move that counts as a changed pixel
WINDOW_M = 3.0      # half-width and half-height of the window on the death point


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
    splash_px_t1 = count("t1.png", "t1-nosplash.png", "t1.json")
    noise_px = count("t1.png", "t1-b.png", "t1.json")
    splash_px_o120 = count("o120.png", "o120-nosplash.png", "o120.json")

    out = {
        "splash_px_t1": splash_px_t1,
        "noise_px": noise_px,
        "splash_px_o120": splash_px_o120,
        "luma_threshold": THRESHOLD,
        "window_m": WINDOW_M,
    }
    ok = splash_px_t1 >= 6 * max(1, noise_px) and splash_px_o120 > 0
    out["pass"] = ok
    with open(os.path.join(HERE, "probe.json"), "w", encoding="utf-8") as f:
        json.dump(out, f, indent=2)
        f.write("\n")
    print(json.dumps(out, indent=2))
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
