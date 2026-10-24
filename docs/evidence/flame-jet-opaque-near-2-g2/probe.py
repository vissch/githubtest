"""Is the white loop out of the jet's orange tip, and is the near third still an opaque body?

Harsh critic r1 (41/100) fix 1, third try (flame-jet-opaque-near-2-g2, 2026-10-24). The owner looked at the
previous round's stills and said the orange tip carries a white loop that reads as a card stroke, not soot. This
file measures that stroke and the three things that could fake its removal.

Measured on three captures of THIS build (`before-1/2/3.png`, copied in from `jet-t2-1/2/3.png`, so their
sidecars' `path` field still reads `jet-t2-N.png`), not guessed: the fire mask spans x 631..1139, 509 px wide;
its LAST THIRD holds 34 948 fire px, of which 3 032 px (8.68%) are white by the rule below. The white is spread
over the whole stream, 5-15% of the fire in every tenth of it, with its peak past the hot third but short of the
tip; even the last tenth measures 0.004. See measurements.md part 2 for the per-tenth bins.

  tip_white_frac_max  white px / fire px over the last third, at the WORST of the three captures. White is
                      luma >= 215 and r - b <= 45: bright and NOT warm, so the cream crescent of a Core card
                      counts and hot orange fire does not. This is the tip bar.
  tip_white_spread    max minus min over the three captures; bar 0.01. The honest answer to "could this have been
                      judged at all" - a number inside the run-to-run noise cannot pass. The clock is pinned
                      (`Flamethrower.PinnedNow`, see measurements.md), so three sessions render the identical
                      frame and this reads 0 by construction, not by luck.
  tip_px_ratio        last-third fire px against the before median (TIP_PX_REF). Must stay in 0.85..1.15, so the
                      stroke cannot be "removed" by deleting, shortening or dimming the tip out of the fire mask.
  near_edge_corr_max  carried over unchanged from `../flame-jet-opaque-near-2/probe.py`: the Pearson correlation
                      between the Sobel edge magnitude of the jet still and of the no-jet plate over the fire's
                      near third, at the worst of the three. An opaque body hides the plate's edges and draws its
                      own; a veil passes them through. Bar 0.15 - the near third stays opaque.

The aim points screen-right by construction, so the nozzle is the fire mask's left edge and the tip its right. The
fire mask is flamecheck.py's own rule, so the two tools agree about what is fire. Same pose, same plate and the
same `Tests/Stills/FlameJetStills.cs` capture as the previous round (one still per batch editor session,
`TW_FLAME_CAP` 1, 2, 3, plate).

Writes probe.json beside itself; nothing in that file is typed by hand. Exits 0 only if all four bars are met.
  usage: python docs/evidence/flame-jet-opaque-near-2-g2/probe.py
         python docs/evidence/flame-jet-opaque-near-2-g2/probe.py --spread   # the before stills only, always 0
"""
import json, os, sys
import numpy as np
from PIL import Image
from scipy import ndimage

HERE = os.path.dirname(os.path.abspath(__file__))
JETS = [os.path.join(HERE, "jet-t2-%d.png" % i) for i in (1, 2, 3)]
BEFORE = [os.path.join(HERE, "before-%d.png" % i) for i in (1, 2, 3)]
PLATE = os.path.join(HERE, "bg-t2.png")

# Set from `probe.py --spread` on the before stills, never by hand: they measure 0.0868, 0.0868, 0.0868 (spread
# 0.0000 - the pinned clock makes three sessions render the same frame). The bar is HALF the smallest of them.
# Halving is what puts it outside that spread: a bar a few thousandths under the baseline would sit inside any
# honest noise band even though this one measures zero.
WHITE_BAR = 0.0434
SPREAD_BAR = 0.01
TIP_PX_REF = 34948          # the before median of last-third fire px, from the same --spread run
PX_LO, PX_HI = 0.85, 1.15
EDGE_BAR = 0.15


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


def thirds(m):
    """The fire mask split along the aim: the near third (nozzle end) and the last third (the orange band)."""
    xs = np.where(m.any(axis=0))[0]
    x0, x1 = int(xs[0]), int(xs[-1])
    span = x1 - x0 + 1
    near = m.copy(); near[:, x0 + span // 3:] = False
    last = m.copy(); last[:, : x1 - span // 3] = False
    return near, last, x0, x1, span


def white(x):
    """Bright and not warm: the cream of a Core card, never hot orange fire."""
    return (luma(x) >= 215) & ((x[..., 0] - x[..., 2]) <= 45)


def corr(a, b):
    a, b = a - a.mean(), b - b.mean()
    d = float(np.sqrt((a * a).sum()) * np.sqrt((b * b).sum()))
    return float((a * b).sum() / d) if d > 1e-9 else 0.0


def measure(path, plate_edges, plate_shape):
    """One still -> (white frac of the last third, its fire px, near-third edge correlation, a printable row)."""
    jet = rgb(path)
    if jet.shape != plate_shape:
        print("%s and the plate are different sizes" % os.path.basename(path), file=sys.stderr)
        return None
    m = firemask(jet)
    if m.sum() < 15000:
        print("no fire in %s - do not measure this capture" % os.path.basename(path), file=sys.stderr)
        return None
    near, last, x0, x1, span = thirds(m)
    if near.sum() < 2000 or last.sum() < 2000:
        print("%s: the stream is too small to split" % os.path.basename(path), file=sys.stderr)
        return None
    w = int((white(jet) & last).sum())
    px = int(last.sum())
    frac = w / float(px)
    c = corr(edges(luma(jet))[near], plate_edges[near])
    row = ("%-14s x %4d..%-4d (%4d px)  near %6d px  last-third %6d px  white %5d px (%.4f)  edge_corr %+.4f"
           % (os.path.basename(path), x0, x1, span, near.sum(), px, w, frac, c))
    return frac, px, c, row


def main(argv):
    spread_only = "--spread" in argv[1:]
    paths = BEFORE if spread_only else JETS
    missing = [p for p in paths + [PLATE] if not os.path.exists(p)]
    if missing:
        print("not captured yet: %s" % ", ".join(os.path.basename(p) for p in missing), file=sys.stderr)
        return 0 if spread_only else 1
    plate = rgb(PLATE)
    pe = edges(luma(plate))
    fracs, pxs, corrs, rows = [], [], [], []
    for path in paths:
        got = measure(path, pe, plate.shape)
        if got is None:
            return 0 if spread_only else 1
        fracs.append(got[0]); pxs.append(got[1]); corrs.append(got[2]); rows.append(got[3])
    for row in rows:
        print(row)
    hi = max(fracs)
    spread = max(fracs) - min(fracs)
    px_med = float(sorted(pxs)[1])
    edge = max(corrs)
    if spread_only:
        print("before tip_white_frac   = %s" % ", ".join("%.4f" % f for f in fracs))
        print("before spread           = %.4f" % spread)
        print("before last-third px    = %s   (median %.0f)" % (", ".join("%d" % p for p in pxs), px_med))
        print("half the smallest       = %.4f   <- what WHITE_BAR must be" % (0.5 * min(fracs)))
        return 0
    ratio = px_med / float(TIP_PX_REF)
    print("tip_white_frac_max     = %.4f   (want <= %.4f, the worst of the three)" % (hi, WHITE_BAR))
    print("tip_white_spread       = %.4f   (want <= %.2f; above it the other numbers mean nothing)"
          % (spread, SPREAD_BAR))
    print("tip_px_ratio           = %.3f    (want %.2f..%.2f against the before median %d px)"
          % (ratio, PX_LO, PX_HI, TIP_PX_REF))
    print("near_edge_corr_max     = %+.4f   (want <= %.2f, the worst of the three)" % (edge, EDGE_BAR))
    out = {
        "tip_white_frac_max": round(hi, 4),
        "tip_white_spread": round(spread, 4),
        "tip_px_ratio": round(ratio, 4),
        "near_edge_corr_max": round(edge, 4),
        "tip_white_frac_each": [round(f, 4) for f in fracs],
        "tip_px_each": pxs,
        "near_edge_corr_each": [round(c, 4) for c in corrs],
        "bars": {"tip_white_frac_max": WHITE_BAR, "tip_white_spread": SPREAD_BAR,
                 "tip_px_ratio": [PX_LO, PX_HI], "near_edge_corr_max": EDGE_BAR},
    }
    with open(os.path.join(HERE, "probe.json"), "w", encoding="utf-8") as f:
        json.dump(out, f, indent=2)
        f.write("\n")
    ok = (spread <= SPREAD_BAR and hi <= WHITE_BAR and PX_LO <= ratio <= PX_HI and edge <= EDGE_BAR)
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main(sys.argv))
