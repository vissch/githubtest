#!/usr/bin/env python
"""Reads the frog's readability off the stills in this folder, the same quantity CaptureRig measures.

CaptureRig.Measure (Editor/CaptureRig.cs) asks, per foot figure: a disc on his chest (radius 0.10 x his drawn
height) against the median of a ring (0.62 - 1.00 x that height) round him, contrast = |man - bg| / (bg + 0.02),
Rec.709 luma on 0..1. It knows where each man is because it asks the sim. This script does not ask anything: it
works from pairs of stills (WITH men, and the same frame of GROUND with nobody in it), so the frog's pixels are
found by what changed and not by what stands out.

    mask   = |(luma(with) - luma(ground)) - median(that difference)| > 0.02   (the median is the frame's own
             exposure shift between the two renders, which is not a man and must not be read as one)
    blobs  = connected components of the mask, one per frog, kept only if they are man-shaped
    man    = median luma of the blob's upper-middle disc, in the WITH still
    bg     = median luma of a ring 0.62 - 1.00 x the blob's height round its centre, in the GROUND still
    contr  = |man - bg| / (bg + 0.02)

It writes probe.json beside itself. Every number comes out of the pixels; nothing is typed in. The unit's bar is
contrast_median >= 0.15; the script reports, it does not enforce (exit 0 whenever it could measure).
"""
import json
import os
import sys

import numpy as np
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
PAIRS = [("night", "frog-night.png", "frog-night-ground.png"),
         ("day", "frog-day.png", "frog-day-ground.png")]
DIFF = 0.02          # the pair threshold: a pixel the men changed
MIN_AREA = 40        # a component smaller than this is sampling noise on an edge, not a man
MIN_TALL = 6.0       # CaptureRig's own rule: a man under 6 px tall says nothing honest
MAX_WIDE = 2.5       # a component wider than this many times its height is ground, not a man
DILATE = 2           # grow the mask by this many pixels before labelling: a man whose middle matches the ground
                     # behind him breaks into pieces, and three pieces of one frog are not three frogs. Men stand
                     # ~35 px apart in these stills and a man is ~20 px wide, so this cannot weld two of them


def luma(path):
    a = np.asarray(Image.open(path).convert("RGB"), dtype=np.float64) / 255.0
    return 0.2126 * a[:, :, 0] + 0.7152 * a[:, :, 1] + 0.0722 * a[:, :, 2]


def grow(mask, r):
    """Dilate by r pixels (a square), so the pieces of one figure join up again."""
    out = mask.copy()
    for _ in range(r):
        g = out.copy()
        g[1:, :] |= out[:-1, :]; g[:-1, :] |= out[1:, :]
        g[:, 1:] |= out[:, :-1]; g[:, :-1] |= out[:, 1:]
        out = g
    return out


def blobs(mask):
    """Connected components of the mask (8-connected), as bounding boxes: one per frog standing clear of the next."""
    h, w = mask.shape
    label = np.zeros((h, w), dtype=np.int32)
    parent = [0]
    nxt = 1
    for y in range(h):
        row, prev = mask[y], label[y - 1] if y else None
        for x in np.nonzero(row)[0]:
            near = []
            if x and label[y, x - 1]:
                near.append(label[y, x - 1])
            if prev is not None:
                for xx in range(max(0, x - 1), min(w, x + 2)):
                    if prev[xx]:
                        near.append(prev[xx])
            if not near:
                parent.append(nxt)
                label[y, x] = nxt
                nxt += 1
            else:
                roots = {find(parent, n) for n in near}
                r = min(roots)
                label[y, x] = r
                for o in roots:
                    parent[o] = r

    boxes = {}
    ys, xs = np.nonzero(label)
    for y, x in zip(ys, xs):
        r = find(parent, label[y, x])
        b = boxes.get(r)
        if b is None:
            boxes[r] = [x, x, y, y, 1]
        else:
            b[0] = min(b[0], x); b[1] = max(b[1], x)
            b[2] = min(b[2], y); b[3] = max(b[3], y); b[4] += 1

    out = []
    for x0, x1, y0, y1, area in boxes.values():
        tall = float(y1 - y0 + 1)
        wide = float(x1 - x0 + 1)
        if area < MIN_AREA or tall < MIN_TALL or wide > tall * MAX_WIDE:
            continue
        out.append((x0, x1 + 1, y0, y1, tall))
    return sorted(out)


def find(parent, i):
    while parent[i] != i:
        parent[i] = parent[parent[i]]
        i = parent[i]
    return i


def disc(img, cy, cx, r):
    h, w = img.shape
    y0, y1 = max(0, cy - r), min(h, cy + r + 1)
    x0, x1 = max(0, cx - r), min(w, cx + r + 1)
    yy, xx = np.mgrid[y0:y1, x0:x1]
    keep = (yy - cy) ** 2 + (xx - cx) ** 2 <= r * r
    return float(np.median(img[y0:y1, x0:x1][keep])) if keep.any() else float("nan")


def ring(img, cy, cx, r_in, r_out):
    h, w = img.shape
    y0, y1 = max(0, cy - r_out), min(h, cy + r_out + 1)
    x0, x1 = max(0, cx - r_out), min(w, cx + r_out + 1)
    yy, xx = np.mgrid[y0:y1, x0:x1]
    d2 = (yy - cy) ** 2 + (xx - cx) ** 2
    keep = (d2 >= r_in * r_in) & (d2 <= r_out * r_out)
    return float(np.median(img[y0:y1, x0:x1][keep])) if keep.any() else float("nan")


def measure(with_png, ground_png):
    a, b = luma(with_png), luma(ground_png)
    if a.shape != b.shape:
        raise SystemExit("the pair is not the same size: %s %s" % (with_png, ground_png))
    d = a - b
    d = d - np.median(d)                                   # the two renders' own exposure shift, taken out
    mask = np.abs(d) > DIFF
    mask = grow(mask, DILATE)
    found = []
    for x0, x1, top, bot, tall in blobs(mask):
        cx = (x0 + x1) // 2
        cy = int(top + tall * 0.42)                        # the upper middle of him: chest, not head and not boots
        r_man = max(2, int(round(tall * 0.10)))
        r_in, r_out = int(round(tall * 0.62)), int(round(tall * 1.00))
        man = disc(a, cy, cx, r_man)
        bg = ring(b, cy, cx, r_in, r_out)                  # the ground he stands against, with nobody in it
        found.append({"x": int(cx), "y": int(cy), "tall_px": tall,
                      "man": round(man, 5), "bg": round(bg, 5),
                      "contrast": round(abs(man - bg) / (bg + 0.02), 5)})
    return found


def main():
    out = {"stills": [], "blobs": []}
    pooled = []
    for name, wit, ground in PAIRS:
        wp, gp = os.path.join(HERE, wit), os.path.join(HERE, ground)
        for p in (wp, gp):
            if not os.path.exists(p):
                raise SystemExit("missing still: " + p)
        found = measure(wp, gp)
        cs = sorted(f["contrast"] for f in found)
        pooled += cs
        out["stills"] += [wit, ground]
        out["blobs"] += [dict(f, pass_=name) for f in found]
        out["contrast_median_" + name] = round(float(np.median(cs)), 5) if cs else -1.0
        out["blob_count_" + name] = len(cs)
        print("%-6s blobs=%d contrast median=%.4f min=%.4f max=%.4f"
              % (name, len(cs), out["contrast_median_" + name], cs[0] if cs else -1, cs[-1] if cs else -1))

    out["contrast_median"] = round(float(np.median(pooled)), 5) if pooled else -1.0
    out["blob_count"] = len(pooled)
    out["diff_threshold"] = DIFF
    print("pooled blobs=%d contrast_median=%.4f (the bar is 0.15)" % (out["blob_count"], out["contrast_median"]))

    # the same quantity CaptureRig read off the same frames, from the sim's own idea of where each man is
    for name, wit, _ in PAIRS:
        side = os.path.join(HERE, wit.replace(".png", ".json"))
        if os.path.exists(side):
            d = json.load(open(side, encoding="utf-8"))
            mine = out["contrast_median_" + name]
            gap = abs(d.get("contrast_median", -1) - mine)
            print("%-6s sidecar contrast_median=%.4f men_in_frame=%s luma_mean=%s; probe %.4f, apart by %.4f%s"
                  % (name, d.get("contrast_median", -1), d.get("men_in_frame"), d.get("luma_mean"), mine, gap,
                     "  <-- over 0.05 apart" if gap > 0.05 else ""))

    with open(os.path.join(HERE, "probe.json"), "w", encoding="utf-8") as f:
        json.dump(out, f, indent=1, sort_keys=True)
    print("wrote " + os.path.join(HERE, "probe.json"))
    return 0


if __name__ == "__main__":
    sys.exit(main())
