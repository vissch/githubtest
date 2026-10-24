"""Measures the blast knockback throw from the stills BlastThrowStills shot, not typed.

Three captures (c1, c2, c3), four moments each, all at the player's standard view (zoom 30, pitch 25, yaw 40) at
night with the rain off: `stand` the row of six standing, `air` the bodies off the ground, `bounce` their first
ground contact, `settled` 1.5 s after the strip, when they have stopped. frames.txt says which strip index each
moment came from and why.

Method, the old probe's (docs/evidence/blast-knockback-death/probe.py) definition so the two numbers compare:
threshold each still against its own background colour to get a foreground mask; the mass present in the earlier
moment and gone in the later one, and the mass present in the later and not in the earlier, each get a centroid;
the distance between those two centroids is the move. What is new is the metres: this set is shot at the player's
oblique view, not top-down, so a pixel is turned into a ground point by intersecting its camera ray with the plane
y = ground_y, read per still from its own sidecar (cam_x/y/z, cam_pitch, cam_yaw, fov and the image size). The old
probe's top-down pixels-per-metre trick is gone.

The burst's flash sits in the air and bounce frames, so pixels brighter than the stand frame's luma_p95 are
dropped before those two centroids are taken.

Scale self-check: the standing men in c1_stand are separate warm pieces against the blue mud; each one's lowest
pixel unprojects to a ground point, and the span from the first to the last, over the gaps between them, must come
out at 3 m within 0.5 m (the old probe's own span/gaps reading of a known row). The 3 m is BlastThrowStills'
`DeathLab.Row(6, x, z, 0, 3f, 100f)` spacing argument, given, not worked out here: off by more and the probe's
metres would mean nothing, so it fails.

Writes probe.json beside this script and exits 1 naming the first line that failed.
"""
import json
import math
import os
import sys

import numpy as np
from PIL import Image
from scipy import ndimage

HERE = os.path.dirname(os.path.abspath(__file__))
CAPTURES = (1, 2, 3)
MOMENTS = ("stand", "air", "bounce", "settled")
ROW_SPACING_M = 3.0          # BlastThrowStills: DeathLab.Row(6, x, z, 0, 3f, 100f)
ROW_SPACING_TOL_M = 0.5
ROW_MEN = 6
# Where the row stands in c1_stand, in pixels: the camera, the spot and the pose are all fixed by the test, so the
# box is. The sixth man is hidden behind a bunker at this angle, so five is enough to read the span.
ROW_ROI = (420, 180, 780, 420)   # x0, y0, x1, y1
ROW_MIN_MEN = 5
ROW_WARM = 4.0                   # R - B: a khaki figure against the blue night mud
ROW_MIN_PIXELS = 200

# The bars, each set below the minimum of the three captures of this build by at least their spread
# (bar = min - max(0.5, max - min)), so no honest scatter between captures can trip it, then clamped at 0: a
# distance below zero is not a bar. The three captures of this build (BlastThrowStills with StripGap 7, air = strip
# 03, bounce = strip 09) scatter wider than their own values, because the changed mass these centroids are taken
# over is the burst's smoke and clods as much as the bodies, and the three spots differ (c1 open mud with a
# bunker, c2 beside the first row's dead, c3 a trench lip). So the rule gives 0 for all three, and these three
# numbers guard nothing yet: what the probe really holds is the sidecar bars and the row-spacing self-check.
# Narrowing them wants a mask that is only bodies; see frames.txt.
BAR_THROW_M = 0.0            # measured 11.22 / 9.59 / 5.06 m -> rule gives -1.10, clamped
BAR_AIR_BOUNCE_M = 0.0       # measured 1.65 / 4.91 / 1.48 m -> rule gives -1.95, clamped
BAR_BOUNCE_SETTLED_M = 0.0   # measured 16.20 / 0.41 / 4.98 m -> rule gives -15.37, clamped

MAX_LUMA_MEAN = 0.30         # night
WANT_ZOOM = 30
WANT_PITCH = 25
# A fallen man lying in the mud is, by design, barely separate from the ground: CaptureRig's contrast check says so
# whenever bodies are in frame, which on a stand frame means the bodies an earlier capture left there. It is the one
# warning this set expects; any other means the shot itself went wrong.
LYING_WARNING = "men barely separate from the ground: unreadable"


def load(cap, moment):
    stem = os.path.join(HERE, "c%d_%s" % (cap, moment))
    side = json.load(open(stem + ".json"))
    rgb = np.array(Image.open(stem + ".png").convert("RGB")).astype(float)
    return side, rgb


def luma(rgb):
    """0-1 Rec.709, the scale CaptureRig's own luma_* use."""
    return (0.2126 * rgb[:, :, 0] + 0.7152 * rgb[:, :, 1] + 0.0722 * rgb[:, :, 2]) / 255.0


def foreground(rgb, thresh=12.0):
    bg = np.median(rgb.reshape(-1, 3), axis=0)
    return np.sqrt(((rgb - bg) ** 2).sum(axis=2)) > thresh


def ground_points(side, px, py, w, h):
    """Pixel columns/rows -> (x, z) on the plane y = ground_y, through the camera the sidecar describes."""
    pitch = math.radians(side["cam_pitch"])
    yaw = math.radians(side["cam_yaw"])
    sp, cp, sy, cy = math.sin(pitch), math.cos(pitch), math.sin(yaw), math.cos(yaw)
    fwd = np.array([sy * cp, -sp, cy * cp])
    right = np.array([cy, 0.0, -sy])
    up = np.array([sy * sp, cp, cy * sp])
    tan_v = math.tan(math.radians(side["fov"]) / 2.0)
    tan_u = tan_v * w / float(h)
    u = ((px + 0.5) / w) * 2.0 - 1.0
    v = 1.0 - ((py + 0.5) / h) * 2.0
    ray = fwd[None, :] + right[None, :] * (u * tan_u)[:, None] + up[None, :] * (v * tan_v)[:, None]
    cam = np.array([side["cam_x"], side["cam_y"], side["cam_z"]])
    t = (side["ground_y"] - cam[1]) / ray[:, 1]
    hit = cam[None, :] + ray * t[:, None]
    return hit[:, 0], hit[:, 2]


def centroid_m(side, mask, w, h):
    py, px = np.nonzero(mask)
    if len(px) == 0:
        return None
    x, z = ground_points(side, px.astype(float), py.astype(float), w, h)
    return float(x.mean()), float(z.mean())


def change_masks(early, late, flash_cut=None):
    """The old probe's two masks: mass that left, and mass that arrived. flash_cut drops the burst's bright pixels."""
    e_rgb, l_rgb = early[1], late[1]
    e_fg, l_fg = foreground(e_rgb), foreground(l_rgb)
    if flash_cut is not None:
        e_fg = e_fg & (luma(e_rgb) <= flash_cut)
        l_fg = l_fg & (luma(l_rgb) <= flash_cut)
    return e_fg & ~l_fg, l_fg & ~e_fg


def move_m(early, late, flash_cut=None):
    h, w = early[1].shape[:2]
    gone, come = change_masks(early, late, flash_cut)
    a = centroid_m(early[0], gone, w, h)
    b = centroid_m(late[0], come, w, h)
    if a is None or b is None:
        return None
    return math.hypot(b[0] - a[0], b[1] - a[1])


def row_spacing(stand):
    """The standing men of c1: warm pieces inside ROW_ROI. Each one's lowest pixel is where he meets the ground; the
    span from the first to the last over the gaps between them is the row's spacing."""
    side, rgb = stand
    h, w = rgb.shape[:2]
    box = np.zeros((h, w), bool)
    box[ROW_ROI[1]:ROW_ROI[3], ROW_ROI[0]:ROW_ROI[2]] = True
    men = box & ((rgb[:, :, 0] - rgb[:, :, 2]) > ROW_WARM)
    lbl, n = ndimage.label(men)
    sizes = ndimage.sum(men, lbl, range(1, n + 1))
    order = [k for k in np.argsort(sizes)[::-1] if sizes[k] >= ROW_MIN_PIXELS][:ROW_MEN]
    if len(order) < ROW_MIN_MEN:
        raise RuntimeError("found %d standing men in c1_stand, want at least %d" % (len(order), ROW_MIN_MEN))
    feet_px, feet_py = [], []
    for k in order:
        ys, xs = np.nonzero(lbl == (k + 1))
        bottom = ys.max()
        feet_px.append(float(xs[ys == bottom].mean()))
        feet_py.append(float(bottom))
    x, z = ground_points(side, np.array(feet_px), np.array(feet_py), w, h)
    pts = sorted(zip(x, z))
    span = math.hypot(pts[-1][0] - pts[0][0], pts[-1][1] - pts[0][1])
    return span / (len(pts) - 1), [(round(a, 2), round(b, 2)) for a, b in pts]


def main():
    out = {"captures": {}, "bars": {"throw_m": BAR_THROW_M, "seg_air_bounce_m": BAR_AIR_BOUNCE_M,
                                    "seg_bounce_settled_m": BAR_BOUNCE_SETTLED_M,
                                    "row_spacing_m": ROW_SPACING_M, "row_spacing_tol_m": ROW_SPACING_TOL_M}}
    fail = []
    shots = {}
    for c in CAPTURES:
        for m in MOMENTS:
            shots[(c, m)] = load(c, m)

    # the sidecar bars: the shot really is the player's view, at night, and the row really is down and drawn
    for c in CAPTURES:
        for m in MOMENTS:
            s = shots[(c, m)][0]
            name = "c%d_%s" % (c, m)
            if s["zoom"] != WANT_ZOOM:
                fail.append("%s: zoom %s, want %d" % (name, s["zoom"], WANT_ZOOM))
            if s["pitch"] != WANT_PITCH:
                fail.append("%s: pitch %s, want %d" % (name, s["pitch"], WANT_PITCH))
            if s["luma_mean"] > MAX_LUMA_MEAN:
                fail.append("%s: luma_mean %.3f, want <= %.2f (night)" % (name, s["luma_mean"], MAX_LUMA_MEAN))
            extra = [x for x in s["warnings"] if x != LYING_WARNING]
            if extra:
                fail.append("%s: warnings %s" % (name, extra))
            if m == "stand":
                if s["men_in_frame"] < ROW_MEN:
                    fail.append("%s: men_in_frame %d, want >= %d" % (name, s["men_in_frame"], ROW_MEN))
            elif s["fallen"] < ROW_MEN:
                fail.append("%s: fallen %d, want >= %d (the row down and drawn)" % (name, s["fallen"], ROW_MEN))

    spacing, men_at = row_spacing(shots[(1, "stand")])
    out["row_spacing_m"] = round(spacing, 3)
    out["row_men_at_xz"] = [[float(a), float(b)] for a, b in men_at]
    if abs(spacing - ROW_SPACING_M) > ROW_SPACING_TOL_M:
        fail.append("c1 row spacing %.2f m, want %.1f +- %.1f: the probe's metres are wrong"
                    % (spacing, ROW_SPACING_M, ROW_SPACING_TOL_M))

    for c in CAPTURES:
        stand, air, bounce, settled = (shots[(c, m)] for m in MOMENTS)
        cut = stand[0]["luma_p95"]
        row = {
            "throw_m": move_m(stand, settled),
            "seg_air_bounce_m": move_m(air, bounce, cut),
            "seg_bounce_settled_m": move_m(bounce, settled, cut),
        }
        for k in row:
            if row[k] is None:
                fail.append("c%d %s: nothing changed between the two stills" % (c, k))
            else:
                row[k] = round(row[k], 3)
        out["captures"]["c%d" % c] = row

    for key, bar in (("throw_m", BAR_THROW_M), ("seg_air_bounce_m", BAR_AIR_BOUNCE_M),
                     ("seg_bounce_settled_m", BAR_BOUNCE_SETTLED_M)):
        for c in CAPTURES:
            v = out["captures"]["c%d" % c][key]
            if v is not None and v < bar:
                fail.append("c%d %s %.2f m, bar %.2f m" % (c, key, v, bar))

    out["failed"] = fail
    with open(os.path.join(HERE, "probe.json"), "w") as f:
        json.dump(out, f, indent=2)
    print("c1 row spacing %.2f m (want %.1f +- %.1f), men at %s"
          % (out["row_spacing_m"], ROW_SPACING_M, ROW_SPACING_TOL_M, out["row_men_at_xz"]))
    for c in CAPTURES:
        r = out["captures"]["c%d" % c]
        print("c%d  throw_m %s  seg_air_bounce_m %s  seg_bounce_settled_m %s"
              % (c, r["throw_m"], r["seg_air_bounce_m"], r["seg_bounce_settled_m"]))
    print("bars: throw_m %.2f  seg_air_bounce_m %.2f  seg_bounce_settled_m %.2f"
          % (BAR_THROW_M, BAR_AIR_BOUNCE_M, BAR_BOUNCE_SETTLED_M))
    if fail:
        print("FAILED:", fail[0], file=sys.stderr)
        return 1
    print("OK")
    return 0


if __name__ == "__main__":
    sys.exit(main())
