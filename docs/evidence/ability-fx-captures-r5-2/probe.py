#!/usr/bin/env python3
"""Pack and measure the dry-ground HE barrage stills, measuring the OLD water plume by its card shape and its
colour distance from SplashTint - decision (a) of 2026-10-19, not by a frame blue share.

WHY A NEW MEASURE. r5 asked for baseline_blue_share_t1 >= 0.10 and measured 0.0183, and the reason was the
measurement, not the picture: the authored BiomeProfile.SplashTint (0.62, 0.70, 0.82) leads green by only 31/255,
under the old 38/255 blue-lead rule, so even a full plume was mostly not "blue". The decision taken (a measuring
choice by the relay coordinator, recorded in docs/reference/decisions.md, 2026-10-20) is to look for what the eye
looks for: a drawn CARD in the plume's own hue.

HOW THE PLUME IS MEASURED (plume_score)
    mask        a pixel where any channel differs from its own plate by more than DIFF (20/255) - the effect itself
    bbox        the burst's box: the 1st-99th percentile of the mask's coordinates per axis, padded BBOX_PAD (8) px
                (r5's rule, unchanged), so a few stray pixels cannot stretch it to the whole frame
    candidate   a mask pixel whose CHROMA - its rgb triple divided by its own sum - is within CHROMA_L1 (0.12) L1 of
                SplashTint's chroma (0.290, 0.327, 0.383). 0.12 is half the chroma distance from SplashTint to
                ColumnTint (0.40, 0.33, 0.26 -> 0.404, 0.333, 0.263), which is 0.241: the midpoint between the
                plume's tint and the earth column it has to be told apart from. Chroma, not raw colour, because the
                night look darkens everything and only the hue survives.
    blobs       4-neighbour connected components of candidates. A blob counts when it has >= BLOB_MIN (40) px,
                fills >= BLOB_FILL (0.35) of its own bounding box, and that box's aspect is between ASPECT_MIN
                (0.25) and ASPECT_MAX (4) - a drawn card is a filled quad, moonlit speckle is not.
    plume_score kept blob pixels / bbox pixels.

ALSO MEASURED
    effect_area             the mask as a share of the whole frame - proof something was actually drawn
    outside_bbox_luma_diff  outside the bbox, the mean |luma(shot) - luma(plate)| on 0-255 Rec.601 (r5's rule). A
                            marker, a HUD element or a global brightening lands outside the burst and shows here. A
                            mask under BBOX_MIN_AREA of the frame has no meaningful bbox: reported 0.0, and the area
                            gate catches that shot instead.

NOISE FIRST (the bar is set outside the measured spread). The old code (TW_BARRAGEDRY_BASELINE=1) and the current
code are each shot THREE times with the same staging, overlays off. The pass, from the unit's card:
    min(baseline_plume_score) - max(barrage_plume_score) >= 3 * max(spread of either triple, 0.01)
    min(baseline_plume_score) >= 0.05
    barrage_effect_area_t1 in 0.05-0.4, barrage_effect_area_o240 >= 0.01, outside_bbox_luma_diff <= 3
If the old plume does not clear this, it is reported as measured. The rule is never loosened.

USAGE (stdlib + PIL only, as Tools/shotstats.py; ASCII only)
    probe.py --pack <capture dir> --tag baseline-1   copy that run's PNGs here as baseline-1-<stem>.png
    probe.py --measure                               measure the committed PNGs -> probe.json, frames.txt
    probe.py --selftest                              two synthetic frames, a hand-counted score

--pack writes PNG (not JPG: the unit's done-when counts *.png), downscaled to PACK_W x PACK_H and PNG-optimised
under MAX_BYTES, with each CaptureRig sidecar beside it as <tag>-<stem>.json. --measure reads only the committed
PNGs. Tags starting with "baseline" are the old code; tags starting with "barrage" are the current code.
"""
import argparse
import glob
import json
import os
import shutil
import sys

from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
MAX_BYTES = 400 * 1024
PACK_W, PACK_H = 800, 450
DIFF = 20                  # 0-255: a channel has to move this much before a pixel counts as the effect
SPLASH_TINT = (0.62, 0.70, 0.82)   # BiomeProfile.SplashTint, the authored water plume
COLUMN_TINT = (0.40, 0.33, 0.26)   # BiomeProfile.ColumnTint, the earth column it must be told apart from
CHROMA_L1 = 0.12           # half the chroma distance between those two tints (0.241)
BLOB_MIN = 40              # px: smaller than this is speckle, not a card
BLOB_FILL = 0.35           # a drawn card fills this much of its own bounding box
ASPECT_MIN, ASPECT_MAX = 0.25, 4.0
BBOX_PAD = 8
BBOX_MIN_AREA = 0.001      # under this there is no burst to put a box around
AREA_MIN, AREA_MAX = 0.05, 0.4
AREA_MIN_O240 = 0.01
PLUME_MIN = 0.05           # min(baseline): the old code's plume has to be visible at all
SPREAD_FLOOR = 0.01        # the spread used for the gap is at least this
GAP_SPREADS = 3            # the gap has to be this many spreads wide
OUTSIDE_DIFF_MAX = 3.0


def chroma(r, g, b):
    s = float(r) + g + b
    if s <= 0:
        return (0.0, 0.0, 0.0)
    return (r / s, g / s, b / s)


SPLASH_CHROMA = chroma(*SPLASH_TINT)


def pack(src, tag):
    """Copy each PNG in src beside this script as <tag>-<stem>.png, downscaled to PACK_W x PACK_H and under
    MAX_BYTES, with its CaptureRig sidecar as <tag>-<stem>.json. Returns the paths written."""
    out = []
    for png in sorted(glob.glob(os.path.join(src, "*.png"))):
        stem = os.path.splitext(os.path.basename(png))[0]
        dst = os.path.join(HERE, "%s-%s.png" % (tag, stem))
        img = Image.open(png).convert("RGB")
        if img.size != (PACK_W, PACK_H):
            img = img.resize((PACK_W, PACK_H), Image.LANCZOS)
        img.save(dst, "PNG", optimize=True)
        if os.path.getsize(dst) > MAX_BYTES:
            img.convert("P", palette=Image.ADAPTIVE, colors=256).save(dst, "PNG", optimize=True)
        out.append(dst)
        side = os.path.splitext(png)[0] + ".json"
        if os.path.exists(side):
            shutil.copyfile(side, os.path.join(HERE, "%s-%s.json" % (tag, stem)))
    return out


def bbox_of(inmask, w, h):
    """The burst's box: the 1st-99th percentile of the mask's coordinates per axis, padded BBOX_PAD. None when the
    mask is too small to put a box around."""
    xs = [i for j in range(h) for i in range(w) if inmask[j * w + i]]
    if len(xs) < BBOX_MIN_AREA * w * h:
        return None
    ys = [j for j in range(h) for i in range(w) if inmask[j * w + i]]
    xs.sort()
    ys.sort()

    def band(v):
        return v[int(0.01 * (len(v) - 1))] - BBOX_PAD, v[int(0.99 * (len(v) - 1))] + BBOX_PAD

    x0, x1 = band(xs)
    y0, y1 = band(ys)
    return max(0, x0), max(0, y0), min(w - 1, x1), min(h - 1, y1)


def blob_pixels(cand, w, h):
    """The pixels in 4-neighbour connected components of `cand` that look like a drawn card: at least BLOB_MIN px,
    filling at least BLOB_FILL of their own bounding box, with an aspect between ASPECT_MIN and ASPECT_MAX."""
    seen = bytearray(w * h)
    kept = 0
    for start in range(w * h):
        if not cand[start] or seen[start]:
            continue
        stack = [start]
        seen[start] = 1
        comp = []
        while stack:
            k = stack.pop()
            comp.append(k)
            i, j = k % w, k // w
            for ni, nj in ((i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)):
                if 0 <= ni < w and 0 <= nj < h:
                    nk = nj * w + ni
                    if cand[nk] and not seen[nk]:
                        seen[nk] = 1
                        stack.append(nk)
        if len(comp) < BLOB_MIN:
            continue
        xs = [k % w for k in comp]
        ys = [k // w for k in comp]
        bw = max(xs) - min(xs) + 1
        bh = max(ys) - min(ys) + 1
        if len(comp) < BLOB_FILL * bw * bh:
            continue
        aspect = bw / float(bh)
        if not (ASPECT_MIN <= aspect <= ASPECT_MAX):
            continue
        kept += len(comp)
    return kept


def diff_row(name, shot, plate):
    """The effect in `shot` against its own plate: how much of the frame it covers, its plume score, and whether
    anything changed outside the burst."""
    a = Image.open(shot).convert("RGB")
    b = Image.open(plate).convert("RGB")
    if a.size != b.size:
        raise SystemExit("%s and its plate are different sizes" % name)
    pa, pb = a.load(), b.load()
    w, h = a.size
    inmask = bytearray(w * h)
    cand = bytearray(w * h)
    mask = 0
    for j in range(h):
        for i in range(w):
            r, g, bl = pa[i, j]
            r0, g0, b0 = pb[i, j]
            if abs(r - r0) <= DIFF and abs(g - g0) <= DIFF and abs(bl - b0) <= DIFF:
                continue
            inmask[j * w + i] = 1
            mask += 1
            cr, cg, cb = chroma(r, g, bl)
            if (abs(cr - SPLASH_CHROMA[0]) + abs(cg - SPLASH_CHROMA[1])
                    + abs(cb - SPLASH_CHROMA[2])) <= CHROMA_L1:
                cand[j * w + i] = 1
    box = bbox_of(inmask, w, h)
    if box is None:
        plume, outside, boxpx = 0.0, 0.0, 0
    else:
        x0, y0, x1, y1 = box
        boxpx = (x1 - x0 + 1) * (y1 - y0 + 1)
        inbox = bytearray(w * h)
        for j in range(y0, y1 + 1):
            for i in range(x0, x1 + 1):
                inbox[j * w + i] = cand[j * w + i]
        plume = blob_pixels(inbox, w, h) / float(boxpx)
        total, count = 0.0, 0
        for j in range(h):
            inrow = y0 <= j <= y1
            for i in range(w):
                if inrow and x0 <= i <= x1:
                    continue
                r, g, bl = pa[i, j]
                r0, g0, b0 = pb[i, j]
                total += abs((0.299 * r + 0.587 * g + 0.114 * bl)
                             - (0.299 * r0 + 0.587 * g0 + 0.114 * b0))
                count += 1
        outside = round(total / count, 3) if count else 0.0
    return {
        "still": name,
        "plate": os.path.basename(plate),
        "effect_area": round(mask / float(w * h), 4),
        "plume_score": round(plume, 4),
        "mask_pixels": mask,
        "bbox_pixels": boxpx,
        "frame_pixels": w * h,
        "outside_bbox_luma_diff": outside,
    }


def pose_errors():
    """Every sidecar's pose_error_m, by still. The plate and the shot share one pose, so the mask is the effect."""
    out = {}
    for j in sorted(glob.glob(os.path.join(HERE, "*.json"))):
        if os.path.basename(j).startswith("probe"):
            continue
        try:
            side = json.load(open(j, encoding="utf-8"))
        except ValueError:
            continue
        if isinstance(side, dict) and "pose_error_m" in side:
            out[os.path.splitext(os.path.basename(j))[0]] = side["pose_error_m"]
    return out


def tags(kind):
    """The tags of the committed runs of one kind ("baseline" or "barrage"), in name order. A tag is a run: its
    barrage-t1 and plate-t1 both have to be here."""
    found = []
    for p in sorted(glob.glob(os.path.join(HERE, "%s*-barrage-t1.png" % kind))):
        tag = os.path.basename(p)[:-len("-barrage-t1.png")]
        if os.path.exists(os.path.join(HERE, "%s-plate-t1.png" % tag)):
            found.append(tag)
    return found


def spread(v):
    return max(v) - min(v) if v else 0.0


def measure():
    rows = []
    scores = {"baseline": [], "barrage": []}
    for kind in ("baseline", "barrage"):
        for tag in tags(kind):
            r = diff_row("%s-barrage-t1" % tag, os.path.join(HERE, "%s-barrage-t1.png" % tag),
                         os.path.join(HERE, "%s-plate-t1.png" % tag))
            rows.append(r)
            scores[kind].append(r["plume_score"])
    o240 = []
    for tag in tags("barrage"):
        shot = os.path.join(HERE, "%s-barrage-o240.png" % tag)
        plate = os.path.join(HERE, "%s-plate-o240.png" % tag)
        if os.path.exists(shot) and os.path.exists(plate):
            r = diff_row("%s-barrage-o240" % tag, shot, plate)
            rows.append(r)
            o240.append(r["effect_area"])
    t1 = [r["effect_area"] for r in rows
          if r["still"].startswith("barrage") and r["still"].endswith("-t1")]
    outside = [r["outside_bbox_luma_diff"] for r in rows
               if r["still"].startswith("barrage") and r["still"].endswith("-t1")]
    sp = max(spread(scores["baseline"]), spread(scores["barrage"]), SPREAD_FLOOR)
    report = {
        "baseline_plume_score": scores["baseline"],
        "barrage_plume_score": scores["barrage"],
        "barrage_effect_area_t1": min(t1) if t1 else 0.0,
        "barrage_effect_area_o240": min(o240) if o240 else 0.0,
        "outside_bbox_luma_diff": max(outside) if outside else 99.0,
        "measured_spread": round(sp, 4),
        "gap": (round(min(scores["baseline"]) - max(scores["barrage"]), 4)
                if scores["baseline"] and scores["barrage"] else 0.0),
        "chroma_l1": CHROMA_L1,
        "splash_chroma": [round(c, 4) for c in SPLASH_CHROMA],
        "column_chroma": [round(c, 4) for c in chroma(*COLUMN_TINT)],
        "plume_min": PLUME_MIN,
        "gap_spreads": GAP_SPREADS,
        "effect_area_min": AREA_MIN,
        "effect_area_max": AREA_MAX,
        "effect_area_min_o240": AREA_MIN_O240,
        "outside_bbox_luma_diff_max": OUTSIDE_DIFF_MAX,
        "pose_error_m": pose_errors(),
        "rows": rows,
    }
    with open(os.path.join(HERE, "probe.json"), "w") as f:
        json.dump(report, f, indent=2, sort_keys=True)
        f.write("\n")

    poses = report["pose_error_m"]
    with open(os.path.join(HERE, "frames.txt"), "w") as f:
        f.write("GreyboxCorridor, NightMud (the owner's night look), clock running, weather pinned at 30 s.\n")
        f.write("The plume is measured by card shape and chroma distance from SplashTint (decision (a),"
                " 2026-10-19), never by a frame blue share.\n")
        for tag in ("baseline", "barrage"):
            mp = os.path.join(HERE, "moments-%s.txt" % tag)
            if os.path.exists(mp):
                f.write(open(mp, encoding="utf-8").read().rstrip("\n") + "\n")
        for r in rows:
            f.write("%s: against %s | effect_area %.4f, plume_score %.4f, outside_bbox_luma_diff %.3f"
                    " | pose_error_m %s\n"
                    % (r["still"], r["plate"], r["effect_area"], r["plume_score"],
                       r["outside_bbox_luma_diff"], poses.get(r["still"], "no sidecar")))

    for r in rows:
        print("  %-28s effect_area %.4f plume_score %.4f outside_bbox_luma_diff %.3f"
              % (r["still"], r["effect_area"], r["plume_score"], r["outside_bbox_luma_diff"]))
    b, a = scores["baseline"], scores["barrage"]
    checks = [
        ("three baseline runs", len(b) >= 3),
        ("three barrage runs", len(a) >= 3),
        ("min(baseline) - max(barrage) >= %d x spread %.4f" % (GAP_SPREADS, sp),
         bool(b) and bool(a) and (min(b) - max(a)) >= GAP_SPREADS * sp),
        ("min(baseline) >= %.2f" % PLUME_MIN, bool(b) and min(b) >= PLUME_MIN),
        ("barrage_effect_area_t1 in %.2f-%.2f" % (AREA_MIN, AREA_MAX),
         AREA_MIN <= report["barrage_effect_area_t1"] <= AREA_MAX),
        ("barrage_effect_area_o240 >= %.2f" % AREA_MIN_O240,
         report["barrage_effect_area_o240"] >= AREA_MIN_O240),
        ("outside_bbox_luma_diff <= %.1f" % OUTSIDE_DIFF_MAX,
         report["outside_bbox_luma_diff"] <= OUTSIDE_DIFF_MAX),
    ]
    for what, good in checks:
        print("  %-48s %s" % (what, "ok" if good else "FAILED"))
    ok = all(good for _, good in checks)
    print("OK" if ok else "NOT OK")
    return 0 if ok else 1


# --- the self test: a worked example, counted from the drawing and not from the code -------------------------
#
# The plate is a flat (40, 40, 45) frame, PACK_W x PACK_H. The shot is that plate with ONE card painted on it:
# 60 x 90 px at (100, 100), in SplashTint x 255 = (158, 178, 209). So:
#   mask        = 60 * 90 = 5400 px, every one of them a candidate (the card IS SplashTint's chroma)
#   bbox x      the mask's xs are 60 columns of 90: the 1st percentile index is int(0.01 * 5399) = 53, which falls
#               in column 100 + 53 // 90 = 100; the 99th is int(0.99 * 5399) = 5345, in column 100 + 5345 // 90 =
#               159. Padded 8: 92 to 167, so 76 px wide.
#   bbox y      the ys are 90 rows of 60: index 53 -> row 100, index 5345 -> row 100 + 5345 // 60 = 189. Padded 8:
#               92 to 197, so 106 px tall.
#   bbox px     76 * 106 = 8056
#   the card    5400 px >= 40, fills 5400 / (60 * 90) = 1.00 >= 0.35, aspect 60 / 90 = 0.67 in 0.25-4: it counts
#   plume_score = 5400 / 8056 = 0.6703
SELFTEST_EXPECTED = 0.6703
CARD_W, CARD_H, CARD_X, CARD_Y = 60, 90, 100, 100


def selftest():
    import tempfile
    tmp = tempfile.mkdtemp(prefix="probe-selftest-")
    plate = Image.new("RGB", (PACK_W, PACK_H), (40, 40, 45))
    shot = plate.copy()
    px = shot.load()
    card = tuple(int(round(c * 255)) for c in SPLASH_TINT)
    for j in range(CARD_Y, CARD_Y + CARD_H):
        for i in range(CARD_X, CARD_X + CARD_W):
            px[i, j] = card
    pp = os.path.join(tmp, "plate-t1.png")
    sp = os.path.join(tmp, "barrage-t1.png")
    plate.save(pp)
    shot.save(sp)
    r = diff_row("selftest", sp, pp)
    print("  card %dx%d of %s on %s" % (CARD_W, CARD_H, card, (40, 40, 45)))
    print("  mask_pixels %d (drawn %d), bbox_pixels %d, plume_score %.4f, expected %.4f"
          % (r["mask_pixels"], CARD_W * CARD_H, r["bbox_pixels"], r["plume_score"], SELFTEST_EXPECTED))
    shutil.rmtree(tmp, ignore_errors=True)
    bad = []
    if r["mask_pixels"] != CARD_W * CARD_H:
        bad.append("the mask is not the card")
    if abs(r["plume_score"] - SELFTEST_EXPECTED) > 0.01:
        bad.append("plume_score %.4f is over 0.01 from the hand-counted %.4f"
                   % (r["plume_score"], SELFTEST_EXPECTED))
    if r["outside_bbox_luma_diff"] != 0.0:
        bad.append("outside the box the two frames are not the same picture")
    for b in bad:
        print("  FAILED: " + b)
    print("OK" if not bad else "NOT OK")
    return 0 if not bad else 1


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--pack", help="the capture folder BarrageDryStills wrote (Captures/barragedry)")
    ap.add_argument("--tag", help="the run's name: baseline-1..3 (the old code) or barrage-1..3 (the current code)")
    ap.add_argument("--measure", action="store_true", help="measure the committed PNGs")
    ap.add_argument("--selftest", action="store_true", help="two synthetic frames against a hand-counted score")
    a = ap.parse_args()
    if a.selftest:
        return selftest()
    if a.pack:
        if not a.tag:
            print("NOT OK: --pack wants a --tag")
            return 1
        for p in pack(a.pack, a.tag):
            print("  packed " + os.path.basename(p))
        return 0
    if a.measure:
        return measure()
    ap.print_help()
    return 1


if __name__ == "__main__":
    sys.exit(main())
