"""Is the flamethrower jet's near third an opaque body, or can you still see the field through it?

Harsh critic r1 (41/100) fix 1, 2026-10-19. The round before this measured `see_through_frac` 0.0000 and the
critic still wrote that the near third is see-through by eye: hedgehogs and stakes read straight through the stream
at x 330-480 of `flame-jet-core/jet-t2-after.png`. The old number was wrong about its own question, in two ways:

  * it called a pixel opaque as soon as the jet tinted it by more than 6/255, and a sheer orange wash over a stake
    does exactly that while leaving the stake perfectly legible;
  * `core_over_tip_luma` came out 1.597 against a bar of 1.5, but four consecutive captures of that same build
    measured 1.526, 1.597, 1.641 and 1.660 - the margin was inside its own run-to-run spread.

What the eye actually reads through a veil is not tint, it is STRUCTURE: the stake's edges. So:

  near_edge_corr          the Pearson correlation between the Sobel edge magnitude of the jet still and of the
                          no-jet plate, over the fire's near third only. An opaque body hides the plate's edges
                          and draws its own, which are unrelated to them, so this falls toward 0; a veil passes
                          them through and it stays high. Measured on the before build for scale, not guessed at.
  core_over_tip_luma_min  the LOWEST of the three captures, not one picked from them, so a number inside the
                          spread cannot pass.

Three jet stills and one plate, all the same pose, all CaptureRig stills with their sidecars beside them:
GreyboxCorridor at night, T2 (zoom 16), yaw 0, pitch 25, focus (100, 104), a 3 s burst from (100, ground, 104)
aimed along screen-right under `Random.InitState(20250925)`, sampled 1.5 s of game time after it is created, the
editor paused and frame-stepped so the sample is a game-time delay and not a wall-clock one, and the weather
pinned (`Atmosphere.PinnedClock` 120) so all four share one sky. The three differ only in the absolute game time
the burst is born at (34, 44, 54 s), which is what the jet's Perlin and `Mathf.Repeat` phases run on.

The aim points screen-right by construction, so the nozzle is the fire mask's left edge and the tip its right.
The fire mask is flamecheck.py's own rule, so the two tools agree about what is fire.

Writes probe.json beside itself; nothing in that file is typed by hand. Exits 0 only if both bars are met.
  usage: python docs/evidence/flame-jet-opaque-near/probe.py
"""
import json, os, sys
import numpy as np
from PIL import Image
from scipy import ndimage

HERE = os.path.dirname(os.path.abspath(__file__))
JETS = [os.path.join(HERE, "jet-t2-%d.png" % i) for i in (1, 2, 3)]
PLATE = os.path.join(HERE, "bg-t2.png")
EDGE_BAR, LUMA_BAR = 0.15, 1.6


def rgb(path):
    return np.asarray(Image.open(path).convert("RGB")).astype(np.float32)


def luma(x):
    return 0.299 * x[..., 0] + 0.587 * x[..., 1] + 0.114 * x[..., 2]


def firemask(x):
    """flamecheck.py's rule: warm and bright, opened and closed, largest connected piece only - the stream, not
    the pool of burning fuel on the ground nor a lick thrown clear of the head."""
    lum, rb = luma(x), x[..., 0] - x[..., 2]
    m = ((rb >= 60) & (lum >= 70)) | ((lum >= 190) & (rb >= 15))
    m = ndimage.binary_closing(ndimage.binary_opening(m, np.ones((3, 3))), np.ones((5, 5)))
    lab, n = ndimage.label(m)
    if n == 0:
        return m
    sizes = ndimage.sum(m, lab, range(1, n + 1))
    return lab == (int(np.argmax(sizes)) + 1)


def edges(lum):
    """Sobel gradient magnitude: where there is structure, whoever drew it."""
    gx = ndimage.sobel(lum, axis=1)
    gy = ndimage.sobel(lum, axis=0)
    return np.hypot(gx, gy)


def near_and_tip(m):
    xs = np.where(m.any(axis=0))[0]
    x0, x1 = int(xs[0]), int(xs[-1])
    span = x1 - x0 + 1
    near = m.copy(); near[:, x0 + span // 3:] = False            # the first third: the nozzle end
    tip = m.copy(); tip[:, : x1 - max(1, span // 6)] = False      # the last sixth: the tip
    return near, tip, x0, x1, span


def corr(a, b):
    a, b = a - a.mean(), b - b.mean()
    d = float(np.sqrt((a * a).sum()) * np.sqrt((b * b).sum()))
    return float((a * b).sum() / d) if d > 1e-9 else 0.0


def main():
    plate = rgb(PLATE)
    pe = edges(luma(plate))
    corrs, ratios, rows = [], [], []
    for path in JETS:
        jet = rgb(path)
        if jet.shape != plate.shape:
            print("%s and the plate are different sizes" % os.path.basename(path), file=sys.stderr)
            return 1
        m = firemask(jet)
        if m.sum() < 15000:
            print("no fire in %s - do not measure this capture" % os.path.basename(path), file=sys.stderr)
            return 1
        near, tip, x0, x1, span = near_and_tip(m)
        if near.sum() < 2000 or tip.sum() < 200:
            print("%s: the stream is too small to split" % os.path.basename(path), file=sys.stderr)
            return 1
        lum = luma(jet)
        c = corr(edges(lum)[near], pe[near])
        r = float(lum[near].mean()) / max(1e-6, float(lum[tip].mean()))
        corrs.append(c); ratios.append(r)
        rows.append("%-14s x %4d..%-4d (%4d px)  near %6d px  tip %5d px  edge_corr %+.4f  core/tip %.3f"
                    % (os.path.basename(path), x0, x1, span, near.sum(), tip.sum(), c, r))
    edge = max(corrs)          # the worst of the three, so one lucky capture cannot carry it
    lo = min(ratios)
    for row in rows:
        print(row)
    print("near_edge_corr         = %+.4f   (want <= %.2f, the worst of the three)" % (edge, EDGE_BAR))
    print("core_over_tip_luma_min = %.3f    (want >= %.2f)" % (lo, LUMA_BAR))
    out = {
        "near_edge_corr": round(edge, 4),
        "core_over_tip_luma_min": round(lo, 4),
        "near_edge_corr_each": [round(c, 4) for c in corrs],
        "core_over_tip_luma_each": [round(r, 4) for r in ratios],
    }
    with open(os.path.join(HERE, "probe.json"), "w", encoding="utf-8") as f:
        json.dump(out, f, indent=2)
        f.write("\n")
    return 0 if (edge <= EDGE_BAR and lo >= LUMA_BAR) else 1


if __name__ == "__main__":
    sys.exit(main())
