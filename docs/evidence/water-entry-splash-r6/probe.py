"""Measures the water entry's splash in the stills beside this file, and writes probe.json.

What is being proved. r2 made the splash wait out a flung body's fall, r5 photographed it, and the harsh critic
scored r5 60/100: the splash was a pale card of about 320 changed pixels in a 14 x 40 px box, the ring did not
register at all, and noise_px 0 came from re-rendering one frozen frame, so "ring_px_land >= 3 x noise" passed by
construction. r6 answers all three: a crown and two wings instead of one card, a ring that opens from a hole, a
noise floor taken from a SECOND LIVE THROW with the same seed, and shots on the spec moments to the frame.

The stills. AgentScripts/water_entry_fling.cs staged them and CaptureRig rendered them, in Play in GreyboxCorridor
at night (the owner's target look), camera T1 - zoom 30, yaw 21, pitch 25 - aimed at the ford, 1600 x 900. A body is
flung off the dry bank at (12, 128) into knee-deep water at (15, 130) with the clock running; fly.y was bisected
until the flight is exactly 43 frames at 1/60 s, so the body and the ring queued with that duration land on one
frame. CaptureRig.Mark named (15, 130) before the shots, so every sidecar carries that point's projection
(mark_px_x/y and mark_px_per_m) and pixels can be turned into metres. timings.txt holds the achieved game-time
delta of every frozen moment, measured by the harness.

  plate.png        the ford with no throw at all, for the record
  pre.png          land - 0.2 s: the body is still in the air
  land.png         land + 0.05 s
  post.png         land + 0.3 s
  land-run2.png    land + 0.05 s of a SECOND throw: same seed, same from/fly/delay, the scene put back to before
                   run 1 (cards cleared, every ring slot parked, pending rings dropped, fallen bodies emptied) and
                   the water clock left pinned across both runs. Its difference from land.png is the splash's own
                   repeatability - the noise floor, measured live.
  land-noring.png  the frozen landing with the ring slots parked but the cards KEPT, so ring_only_px says in pixels
                   how much of the landing frame is the ring alone.
  <name>-nosplash.png  for each of pre, land and post: that SAME frozen instant rendered again with the water entry
                   lifted out - WaterRings' ring array parked and FlipbookFx's card list emptied - nothing else
                   touched.

Why each moment has its own reference instead of being compared against the plate: the water's foam and lapping are
animated from the water clock in TWWater.hlsl, so a still taken a second after the plate differs from it across the
whole water sheet, about 1,600 changed pixels in the disc at the landing point whether anything landed there or not.
The first run of r5's probe measured ring_px_pre 1577 against ring_px_land 1703 and could not tell the two apart. A
reference rendered at the same frozen instant with the splash lifted out has the same foam, the same body, the same
shadows and the same sky, so the difference between the pair is the water entry and nothing else.

The region. An upright 6 m window on the landing point: within 3 m across the view of the mark, and from 3 m of
height above the mark's own pixel row down to 3 m below it. A water entry is two things - a ring lying flat ON the
surface and a crown standing UP out of it - so the region has to hold both. It does not have to exclude the body,
the cards or anything else, because the reference already removes the splash alone.

The centroid is measured ACROSS the view only (the pixel column against mark_px_x). mark_px_per_m converts ground
metres; a splash stands up out of the water, and its height in pixels is not a distance along the ground, so turning
the vertical offset into metres with the same number would report a splash that is exactly on its mark as a metre
away from it. Measured across the view, "did it ring where he landed" is answered on the ground where it is asked.

A pixel counts as changed when its Rec.709 luma (0..1, the luma CaptureRig's own numbers use) moves by more than
0.02. The threshold is r5's, unchanged: it is never raised to make a ratio pass.

What it writes:
  ring_px_pre          changed pixels at land - 0.2 s: the splash that has not happened yet
  ring_px_land         changed pixels at land + 0.05 s: the splash
  noise_px             changed pixels between the two live throws' landing frames
  noise_source         "second_throw"
  ring_px_post         changed pixels at land + 0.3 s
  ring_only_px         changed pixels the RING alone accounts for at the landing (land against land-noring)
  ring_centroid_off_m  how far across the view the splash's centroid at the landing sits from the landing point
  ring_peak_luma_*     the largest luma move of each pair, so a count of 0 reads as "nothing there" rather than
                       "just under the threshold"
  t_pre_s, t_land_s, t_post_s, t_land_run2_s   the achieved moments, read from timings.txt

Pass: ring_px_pre <= noise_px, ring_px_land >= 1000 and >= 3 * noise_px, ring_px_post >= 600,
ring_centroid_off_m <= 0.5 - the owner's own bars.

Run: python docs/evidence/water-entry-splash-r6/probe.py
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


def timings():
    """The achieved moments, as the harness measured them: `<name> <delta>` lines in timings.txt."""
    out = {}
    with open(os.path.join(HERE, "timings.txt"), encoding="utf-8") as f:
        for line in f:
            bits = line.split()
            if len(bits) == 2:
                try:
                    out[bits[0]] = float(bits[1])
                except ValueError:
                    pass
    return out


def main():
    side = json.load(open(os.path.join(HERE, "land.json"), encoding="utf-8"))
    cx, cy, ppm = side["mark_px_x"], side["mark_px_y"], side["mark_px_per_m"]

    pre, w, h = luma("pre.png")
    pre_ref, _, _ = luma("pre-nosplash.png")
    land, _, _ = luma("land.png")
    land_ref, _, _ = luma("land-nosplash.png")
    land_noring, _, _ = luma("land-noring.png")
    land_run2, _, _ = luma("land-run2.png")
    post, _, _ = luma("post.png")
    post_ref, _, _ = luma("post-nosplash.png")

    idx = window(w, h, cx, cy, WINDOW_M * ppm)
    pre_n, _, pre_peak = diff(pre, pre_ref, idx, w)
    land_n, land_col, land_peak = diff(land, land_ref, idx, w)
    noise_n, _, noise_peak = diff(land, land_run2, idx, w)
    only_n, _, only_peak = diff(land, land_noring, idx, w)
    post_n, _, post_peak = diff(post, post_ref, idx, w)

    off = 9.0 if land_col is None else abs(land_col - cx) / ppm
    t = timings()

    out = {
        "ring_px_pre": pre_n,
        "ring_px_land": land_n,
        "noise_px": noise_n,
        "noise_source": "second_throw",
        "ring_px_post": post_n,
        "ring_only_px": only_n,
        "ring_centroid_off_m": round(off, 3),
        "ring_peak_luma_pre": round(pre_peak, 4),
        "ring_peak_luma_land": round(land_peak, 4),
        "ring_peak_luma_post": round(post_peak, 4),
        "ring_peak_luma_noise": round(noise_peak, 4),
        "ring_peak_luma_only": round(only_peak, 4),
        "t_pre_s": t.get("pre"),
        "t_land_s": t.get("land"),
        "t_post_s": t.get("post"),
        "t_land_run2_s": t.get("land-run2"),
        "flight_s": t.get("flight_s"),
        "flight_frames": t.get("flight_frames"),
        "region_px": len(idx),
        "mark_px_x": cx,
        "mark_px_y": cy,
        "mark_px_per_m": ppm,
        "luma_threshold": THRESHOLD,
        "window_m": WINDOW_M,
    }
    ok = (pre_n <= noise_n and land_n >= max(1000, 3 * noise_n) and post_n >= 600 and off <= 0.5)
    out["pass"] = ok
    with open(os.path.join(HERE, "probe.json"), "w", encoding="utf-8") as f:
        json.dump(out, f, indent=2)
        f.write("\n")
    print(json.dumps(out, indent=2))
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
