"""C73: white-out and hue in night streak bundles, measured on C71's held-clock frames (runs/9/c71f-1*.png).

Usage: python c73.py [--dir DIR] [--label c71f-1] [--debug OUTDIR]   (--debug writes a mask overlay per frame)

Colours (CombatFx.cs at HEAD): halo A (green side) additive (0.06, 0.36, 0.12), halo B (red side) additive
(0.50, 0.07, 0.05), core opaque unlit (3.0, 2.7, 2.3), which lands as pure 255 white. So a raw "luma >= 0.98 in a
bundle" count is mostly core, and C74 (BlendOp Max on the two halo materials) cannot change a core pixel.

Detector, per frame:
  warm      : R >= G >= B, R-B >= 0.15, G-B >= 0.06 (lamps, fires, muzzle and burst flashes)
  halo px   : green hue (G - max(R,B) >= 0.15) or red hue (R - max(G,B) >= 0.20, not warm), and luma >= 0.12
  white px  : luma >= 0.98 (Rec.709 on 8-bit sRGB values)
  flash     : a white px with more warm px (at >= 2 px from white, so a core's 1 px edge is not glow) than halo px
              in its 15x15 window. Dropped, so lamps and muzzle or burst flashes do not count
  bundle    : 8-connected component of (halo | white), dilated by 1 px so a core joins its own halo; kept only if it
              holds >= 60 halo px, halo px are >= 15% of it, and it is longer than wide (covariance eigenvalue
              ratio >= 2; a lamp's glow disc is ~1). Its side is the majority of its halo px
  wide white: white px that survive an opening of the white mask with a 7x7 disc: wider than one core (1-4 px)
  halo-only : bundle px at >= 2 px from any white px (no core, no core edge). If the halos whitened anything by
              stacking, these px would reach luma 0.98. This is the number that decides C74
  edge ring : halo px of the bundle at 1-2 px from its white. G-R there (0..1) is the hue at the halo edge
Verified by overlay (--debug) on frames 0, 8, 28, 29, 31: lamps and the pillbox muzzle flash fall out as flash, slabs
by the pillbox and in the left fan are inside bundles. Known misses, all conservative for white: a few core px on
thin red lanes and two lamp-glow slivers per frame. Needs Pillow, numpy and scipy (present on the bench machine).
"""
import argparse, glob, os, re, sys
import numpy as np
from PIL import Image
from scipy import ndimage as ndi

HERE = os.path.dirname(os.path.abspath(__file__))


def disc(r):
    y, x = np.mgrid[-r:r + 1, -r:r + 1]
    return x * x + y * y <= r * r + 0.5


def measure(path, debug_dir=None):
    a = np.asarray(Image.open(path).convert("RGB")).astype(np.float32) / 255.0
    R, G, B = a[..., 0], a[..., 1], a[..., 2]
    luma = 0.2126 * R + 0.7152 * G + 0.0722 * B
    white = luma >= 0.98
    warm = (R >= G) & (G >= B) & (R - B >= 0.15) & (G - B >= 0.06)          # lamp, fire, flash, burst glow
    green = (G - np.maximum(R, B) >= 0.15) & (luma >= 0.12)
    red = (R - np.maximum(G, B) >= 0.20) & ~warm & (luma >= 0.12)
    halo = green | red
    # a white px with more warm glow than halo within 7 px is part of a flash or a lamp, not a streak core
    k = 15
    glow = warm & ~ndi.binary_dilation(white, structure=np.ones((5, 5), bool))   # a core's 1 px warm edge is not glow
    flash = white & (ndi.uniform_filter(glow.astype(np.float32), k) > ndi.uniform_filter(halo.astype(np.float32), k))
    white = white & ~flash
    streak = ndi.binary_dilation(halo | white, structure=np.ones((3, 3), bool))
    lab, n = ndi.label(streak, structure=np.ones((3, 3), bool))
    idx = np.arange(1, n + 1)
    size = ndi.sum(np.ones_like(luma), lab, idx)
    nhalo = ndi.sum(halo, lab, idx)
    ngreen = ndi.sum(green, lab, idx)
    nred = ndi.sum(red, lab, idx)
    keep = (nhalo >= 60) & (nhalo >= 0.15 * size)
    for i in np.nonzero(keep)[0]:                        # a streak bundle is long; a lamp's glow disc is round
        ys, xs = np.nonzero(lab == (i + 1))
        ev = np.linalg.eigvalsh(np.cov(np.vstack([xs, ys]).astype(np.float64)))
        keep[i] = ev[0] <= 0 or ev[1] / ev[0] >= 2.0     # variance ratio: a glow disc is ~1
    wide = ndi.binary_opening(white, structure=disc(3))
    near_core = ndi.binary_dilation(white, structure=np.ones((5, 5), bool)) & ~white   # 1-2 px outside the core
    near_slab = ndi.binary_dilation(wide, structure=np.ones((5, 5), bool)) & ~white     # 1-2 px outside a wide slab
    bundles = []
    kept_mask = np.zeros(lab.shape, bool)
    for i in np.nonzero(keep)[0]:
        m = lab == (i + 1)
        kept_mask |= m
        w = int((white & m).sum())
        ring = near_core & m & halo
        gr = (G - R)[ring]
        ys, xs = np.nonzero(m)
        bundles.append({
            "side": "green" if ngreen[i] >= nred[i] else "red",
            "px": int(size[i]), "white": w, "wide": int((wide & m).sum()),
            "edge_gr": float(gr.mean()) if gr.size else float("nan"), "edge_n": int(gr.size),
            "slab_gr": float((G - R)[near_slab & m & halo].mean()) if (near_slab & m & halo).any() else float("nan"),
            "box": (int(xs.min()), int(ys.min()), int(xs.max()), int(ys.max())),
        })
    pure = (a >= 1.0).all(axis=2)
    # the halo ceiling: bundle px at >= 2 px from any white px are halo (and ground) only, no core, no core edge
    far = kept_mask & ~ndi.binary_dilation(white | flash, structure=np.ones((5, 5), bool))
    fl = luma[far]
    gonly = far & green & ~ndi.binary_dilation(red, structure=np.ones((5, 5), bool))
    stats = {"wide_pure": int((wide & kept_mask & pure).sum()), "wide_all": int((wide & kept_mask).sum()),
             "offcore_white": int((white & kept_mask & ~ndi.binary_dilation(pure, structure=np.ones((3, 3), bool))).sum()),
             "flash_white": int(flash.sum()),
             "halo_max_luma": float(fl.max()) if fl.size else 0.0, "halo_n_098": int((fl >= 0.98).sum()),
             "halo_n_090": int((fl >= 0.90).sum()), "green_max_r": float(R[gonly].max()) if gonly.any() else 0.0}
    dropped_white = int((white & ~kept_mask).sum())
    dropped_wide = int((wide & ~kept_mask).sum())
    if debug_dir:
        base = os.path.splitext(os.path.basename(path))[0]
        img = (a * 255).astype(np.uint8)
        ov = (img * 0.35).astype(np.uint8)
        ov[kept_mask & ~white] = (0, 90, 160)           # bundle, not white: blue
        ov[white & kept_mask] = (255, 255, 255)          # white inside a bundle
        ov[white & ~kept_mask] = (255, 160, 0)           # white outside every bundle: orange
        ov[flash] = (255, 60, 0)                         # white the detector called flash or lamp: red-orange
        ov[wide & kept_mask] = (255, 0, 255)             # wide white in a bundle: magenta
        ov[wide & ~kept_mask] = (255, 255, 0)            # wide white outside bundles: yellow
        Image.fromarray(ov).save(os.path.join(debug_dir, base + ".mask.png"))
    return bundles, dropped_white, dropped_wide, stats


def frames(d, label):
    fs = glob.glob(os.path.join(d, label + ".png")) + glob.glob(os.path.join(d, label + ".f*.png"))
    fs = [f for f in fs if re.search(r"(\.png|\.f\d+\.png)$", f) and ".mask." not in f]
    key = lambda f: int(re.search(r"\.f(\d+)\.png$", f).group(1)) if re.search(r"\.f(\d+)\.png$", f) else 0
    return sorted(fs, key=key)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--dir", default=HERE)
    ap.add_argument("--label", default="c71f-1")
    ap.add_argument("--debug", default=None, help="write per-frame mask overlays here")
    a = ap.parse_args()
    rows = []
    print("| frame | bundles g/r | white px in bundles | max white/bundle (raw, incl. cores) | max wide white/bundle "
          "| wide white pure 255 | white >1 px from pure | G-R at edge, green (mean, lowest bundle) | G-R beside widest slab "
          "| R-G at edge, red | halo-only px: max luma, n >= 0.90, n >= 0.98 | green-only halo max R "
          "| white called flash/lamp | white outside bundles |")
    print("|---|---|---|---|---|---|---|---|---|---|---|---|---|---|")
    for f in frames(a.dir, a.label):
        b, dw, dwide, st = measure(f, a.debug)
        g = [x for x in b if x["side"] == "green"]
        r = [x for x in b if x["side"] == "red"]
        wg = [x for x in g if x["edge_n"] >= 20]
        gr_mean = (sum(x["edge_gr"] * x["edge_n"] for x in wg) / sum(x["edge_n"] for x in wg)) if wg else float("nan")
        gr_min = min((x["edge_gr"] for x in wg), default=float("nan"))
        wr = [x for x in r if x["edge_n"] >= 20]
        rg_mean = -(sum(x["edge_gr"] * x["edge_n"] for x in wr) / sum(x["edge_n"] for x in wr)) if wr else float("nan")
        mw = max(b, key=lambda x: x["white"], default=None)
        mwide = max(b, key=lambda x: x["wide"], default=None)
        name = os.path.basename(f)
        rows.append(dict(name=name, maxwhite=mw["white"] if mw else 0, maxwide=mwide["wide"] if mwide else 0,
                         gr=gr_mean, grmin=gr_min, slab=mwide["slab_gr"] if mwide and mwide["wide"] else float("nan"),
                         st=st))
        print("| %s | %d/%d | %d | %s | %s | %s | %d | %.2f, %.2f | %s | %.2f | %.3f, %d, %d | %.2f | %d | %d |" % (
            name.replace("c71f-1", "").lstrip(".") or "shot", len(g), len(r), sum(x["white"] for x in b),
            "%d %s %s" % (mw["white"], mw["side"], mw["box"]) if mw else "-",
            "%d %s %s" % (mwide["wide"], mwide["side"], mwide["box"]) if mwide and mwide["wide"] else "0",
            "%.0f%%" % (100.0 * st["wide_pure"] / st["wide_all"]) if st["wide_all"] else "-", st["offcore_white"],
            gr_mean, gr_min, "%.2f" % rows[-1]["slab"] if rows[-1]["slab"] == rows[-1]["slab"] else "-",
            rg_mean, st["halo_max_luma"], st["halo_n_090"], st["halo_n_098"], st["green_max_r"], st["flash_white"], dw))
    if rows:
        print()
        print("max raw white in one bundle (cores included): %d" % max(r["maxwhite"] for r in rows))
        print("max wide white in one bundle (> a core wide) : %d, frames with >= 50: %d of %d" % (
            max(r["maxwide"] for r in rows), sum(r["maxwide"] >= 50 for r in rows), len(rows)))
        print("wide white that is pure 255 (all frames)     : %.1f%%" % (
            100.0 * sum(r["st"]["wide_pure"] for r in rows) / max(1, sum(r["st"]["wide_all"] for r in rows))))
        print("max white >1 px from any pure-255 px, 1 frame: %d" % max(r["st"]["offcore_white"] for r in rows))
        print("halo-only px (>= 2 px from white): max luma %.4f, px at luma >= 0.98 in all frames %d, >= 0.90 %d" % (
            max(r["st"]["halo_max_luma"] for r in rows), sum(r["st"]["halo_n_098"] for r in rows),
            sum(r["st"]["halo_n_090"] for r in rows)))
        print("green-only halo px (no red lane within 2 px): max R %.3f" % max(r["st"]["green_max_r"] for r in rows))
        print("G-R at green halo edge, mean over frames     : %.3f (lowest bundle %.3f)" % (
            np.nanmean([r["gr"] for r in rows]), np.nanmin([r["grmin"] for r in rows])))
        print("G-R beside the widest slab, green frames     : mean %.3f, min %.3f" % (
            np.nanmean([r["slab"] for r in rows]), np.nanmin([r["slab"] for r in rows])))


if __name__ == "__main__":
    sys.exit(main())
