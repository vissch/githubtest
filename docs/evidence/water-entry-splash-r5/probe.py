"""Measures the water entry's splash in the stills beside this file, and writes probe.json.

What is being proved. r2 made the splash wait out a flung body's fall (WaterEntry.Throw queues the ring and the
Splash card with the flight's own duration, WaterEntry.Pump opens the ring at the landing). r2 proved that with
Pump(0.4f)/Pump(0.45f) arithmetic only, and the harsh critic scored it 15/100: arithmetic cannot say whether
anything appears on the water, where, or when. This counts the pixels of real stills instead.

The stills. AgentScripts/water_entry_fling.cs staged them and CaptureRig rendered them, in Play in GreyboxCorridor
at night (the owner's target look), camera T1 - zoom 30, yaw 21, pitch 25 - aimed at the ford, 1600 x 900. A body is
flung off the dry bank at (12, 128) into knee-deep water at (15, 130) with the clock running; the flight is 0.721 s.
CaptureRig.Mark named (15, 130) before the shots, so every sidecar carries that point's projection (mark_px_x/y and
mark_px_per_m) and pixels can be turned into metres.

  plate.png        the ford with no throw at all, for the record
  pre.png          0.217 s BEFORE the body reaches the water: it is still in the air
  land.png         0.033 s AFTER it reaches the water
  land-b.png       the same frozen world rendered a second time
  post.png         0.283 s after the landing
  <name>-nosplash.png  for each of pre, land and post: that SAME frozen instant rendered again with the water entry
                   lifted out - WaterRings' ring array emptied and FlipbookFx's card list emptied - and nothing else
                   touched.

Why each moment has its own reference instead of being compared against the plate: the water's foam and lapping are
animated from `_Time.y` in TWWater.hlsl, so a still taken a second after the plate differs from it across the whole
water sheet, about 1,600 changed pixels in the disc at the landing point whether anything landed there or not. The
first run of this probe measured ring_px_pre 1577 against ring_px_land 1703 and could not tell the two apart. A
reference rendered at the same frozen instant with the splash lifted out has the same foam, the same body, the same
shadows and the same sky, so the difference between the pair is the water entry and nothing else.

The region. An upright 6 m window on the landing point: within 3 m across the view of the mark, and from 3 m of
height above the mark's own pixel row down to 3 m below it. A water entry is two things - a ring lying flat ON the
surface and a Splash card standing UP out of it - so the region has to hold both. It does not have to exclude the
body, the card or anything else, because the reference already removes the splash alone.

The centroid is measured ACROSS the view only (the pixel column against mark_px_x). mark_px_per_m converts ground
metres; a splash stands up out of the water, and its height in pixels is not a distance along the ground, so turning
the vertical offset into metres with the same number would report a splash that is exactly on its mark as a metre
away from it. Measured across the view, "did it ring where he landed" is answered on the ground where it is asked.

A pixel counts as changed when its Rec.709 luma (0..1, the luma CaptureRig's own numbers use) moves by more
than 0.02. The noise floor pair never reaches that: its largest luma move anywhere in the region is 0.011.

What it writes:
  ring_px_pre          changed pixels at land - 0.217 s: the splash that has not happened yet
  ring_px_land         changed pixels at land + 0.033 s: the splash
  noise_px             changed pixels between the two renders of the identical frozen landing frame
  ring_px_post         changed pixels at land + 0.283 s, for the record
  ring_centroid_off_m  how far across the view the splash's centroid at the landing sits from the landing point
  ring_peak_luma_*     the largest luma move of each pair, so a count of 0 can be read as "nothing there" rather
                       than "just under the threshold"

Pass: ring_px_pre <= noise_px, ring_px_land >= 100 and >= 3 * noise_px, ring_centroid_off_m <= 0.5.

Run: python docs/evidence/water-entry-splash-r5/probe.py
"""
import json
import os
import sys

from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
THRESHOLD = 0.02      # luma move that counts as a changed pixel
WINDOW_M = 3.0        # half-width and half-height of the window on the landing point


def luma(name):
    """Rec.709 luma, 0..1, as a row-major list read from the image's top-left - the way the sidecar counts pixels."""
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
    """Indices of the pixels in the upright window on the landing point."""
    idx = []
    for y in range(max(0, int(cy - reach)), min(h - 1, int(cy + reach)) + 1):
        row = y * w
        for x in range(max(0, int(cx - reach)), min(w - 1, int(cx + reach)) + 1):
            idx.append(row + x)
    return idx


def diff(a, b, idx, w):
    """Changed pixels in the window, their mean column, and the largest luma move seen."""
    n = 0
    sx = 0.0
    peak = 0.0
    for i in idx:
        d = abs(a[i] - b[i])
        if d > peak:
            peak = d
        if d > THRESHOLD:
            n += 1
            sx += i % w
    return n, (sx / n if n else None), peak


def main():
    side = json.load(open(os.path.join(HERE, "land.json"), encoding="utf-8"))
    cx, cy, ppm = side["mark_px_x"], side["mark_px_y"], side["mark_px_per_m"]

    pre, w, h = luma("pre.png")
    pre_ref, _, _ = luma("pre-nosplash.png")
    land, _, _ = luma("land.png")
    land_ref, _, _ = luma("land-nosplash.png")
    land_b, _, _ = luma("land-b.png")
    post, _, _ = luma("post.png")
    post_ref, _, _ = luma("post-nosplash.png")

    idx = window(w, h, cx, cy, WINDOW_M * ppm)
    pre_n, _, pre_peak = diff(pre, pre_ref, idx, w)
    land_n, land_col, land_peak = diff(land, land_ref, idx, w)
    noise_n, _, noise_peak = diff(land, land_b, idx, w)
    post_n, _, post_peak = diff(post, post_ref, idx, w)

    off = 9.0 if land_col is None else abs(land_col - cx) / ppm

    out = {
        "ring_px_pre": pre_n,
        "ring_px_land": land_n,
        "noise_px": noise_n,
        "ring_px_post": post_n,
        "ring_centroid_off_m": round(off, 3),
        "ring_peak_luma_pre": round(pre_peak, 4),
        "ring_peak_luma_land": round(land_peak, 4),
        "ring_peak_luma_post": round(post_peak, 4),
        "ring_peak_luma_noise": round(noise_peak, 4),
        "region_px": len(idx),
        "mark_px_x": cx,
        "mark_px_y": cy,
        "mark_px_per_m": ppm,
        "luma_threshold": THRESHOLD,
        "window_m": WINDOW_M,
    }
    ok = (pre_n <= noise_n and land_n >= 100 and land_n >= 3 * noise_n and off <= 0.5)
    out["pass"] = ok
    with open(os.path.join(HERE, "probe.json"), "w", encoding="utf-8") as f:
        json.dump(out, f, indent=2)
        f.write("\n")
    print(json.dumps(out, indent=2))
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
