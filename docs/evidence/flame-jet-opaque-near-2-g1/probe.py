"""Is the flamethrower jet's near third ONE opaque sheet, with no cut through it?

Harsh critic r1 fix 1, third try (flame-jet-opaque-near-2-g1, 2026-10-24). The owner read the last round's
evidence and found two faults in it (his words):

  * "jet-t2-1, jet-t2-2 and jet-t2-3 are the same png ... edge_spread 0 is that one frame counted three times."
    The pinned clock fixed the jitter by pinning ONE age (1.5 s) for all three sessions, so the three captures
    rendered byte-identical images and the repeatability bar was met by construction. `FlameJetStills.cs` now shoots
    three AGES of that pinned clock (1.3, 1.5, 1.7 s), and this probe REFUSES to measure three frames that are not
    distinct: identical sha256, or a zero maximum channel difference, and it exits non-zero saying so.
  * "A pale straight cut runs through the underside of the white core, one card meeting the next." That cut is what
    `seam_score` measures, below.

What is measured:

  near_edge_corr_max      the Pearson correlation between the Sobel edge magnitude of the jet still and of the
                          no-jet plate, over the fire's near third only, at the WORST of the three captures. An
                          opaque body hides the plate's edges and draws its own, unrelated to them, so this falls
                          toward 0; a veil passes them through and it stays high.
  seam_score              the owner's cut, as a number, and INTERNAL only. The owner's fault is a line "through the
                          underside of the white core", so the near third's mask is eroded by 13x13 (about 6 px in
                          from the fire mask's outline) and nothing on that outline is measured - a lone card's own
                          hard rim is not a cut. Inside that region a straight internal boundary concentrates Sobel
                          energy on one LINE: for angles -90..88 step 2 the inner edge magnitude is rotated
                          (order 1), and over every row whose covered run is at least half the inner width we take
                          `row mean / inner mean`; seam_score is the largest such ratio over all angles, reported as
                          the worst (largest) of the three captures. One sheet has no internal straight line; two
                          cards meeting have one. Round 2 measured the outline instead, and so answered a question
                          nobody asked: all six captures peaked at exactly 0 degrees, the aim axis, and width,
                          length and alpha moved the score by 0.5% "because the rim IS the card".
  core_over_tip_luma_min  the LOWEST of the three captures, so a number inside the spread cannot pass.

Where the bars come from. Not from judgement: from the CURRENT build. `baseline/` holds three stills of the build
as it was before this round's look change, shot at the same three ages by the same harness, plus its own plate.
The probe measures them too, and exits non-zero unless EDGE_BAR is below the baseline's minimum near_edge_corr and
SEAM_BAR below the baseline's minimum seam_score - so a bar that sits inside the current build's own spread cannot
be passed off as a pass. The previous round's `edge_spread` bar is gone: only identical frames could meet it.

The measured baseline triples (this build, 2026-10-24; see measurements.md):
  near_edge_corr   0.0172, 0.0065, 0.0153   min 0.0065 -> EDGE_BAR 0.0032
  seam_score       2.0883, 2.0303, 2.1273   min 2.0303 -> SEAM_BAR 1.01   (eroded: internal lines only)
  core_over_tip    1.9610, 2.0237, 2.0029   min 1.9610 (already over LUMA_BAR)
LUMA_BAR stays 1.6, the owner's standing bar from the round before.

Pose and clock, all four stills (`Tests/Stills/FlameJetStills.cs`, one SESSION per still, TW_FLAME_CAP = 1, 2, 3,
plate): GreyboxCorridor at night, T2 (zoom 16), yaw 0, pitch 25, focus (100, 104), a 3 s burst from
(100, ground + 1.1, 104) aimed along screen-right under `Random.InitState(20250925)`, the world held
(`CaptureRig.Hold(120)`) and the jet's clock pinned with `Flamethrower.PinnedNow`: born at 100 s, photographed at
101.3 / 101.5 / 101.7 s. The aim points screen-right by construction, so the nozzle is the fire mask's left edge
and the tip its right. The fire mask is flamecheck.py's own rule, so the two tools agree about what is fire.

Writes probe.json beside itself; nothing in that file is typed by hand. Every number is printed.
  usage: python docs/evidence/flame-jet-opaque-near-2-g1/probe.py [--baseline]
"""
import hashlib, json, os, sys
import numpy as np
from PIL import Image
from scipy import ndimage

HERE = os.path.dirname(os.path.abspath(__file__))
# Both bars are the midpoint between the baseline minimum and 0, rounded down, so each sits strictly outside the
# current build's own measured spread. EDGE_BAR keeps four decimals: the baseline's own minimum is 0.0065, so two
# decimals would round the bar to 0.00 and no build could ever meet it. SEAM_BAR was re-derived by that same rule
# when seam_score changed to measure internal lines only: the eroded baseline minimum is 2.0303, half is 1.01515,
# rounded down 1.01. The rule did not move, the metric under it did; bars_outside_baseline_spread still refuses any
# bar sitting inside the current build's own spread.
EDGE_BAR, SEAM_BAR, LUMA_BAR = 0.0032, 1.01, 1.6


def paths(root):
    return [os.path.join(root, "jet-t2-%d.png" % i) for i in (1, 2, 3)], os.path.join(root, "bg-t2.png")


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


def seam_score(em, near):
    """The strongest straight line of edge energy INSIDE the fire, at any angle - never its own outline.

    The near third's mask is first eroded by 13x13, about 6 px in from the fire mask's outline, and everything is
    measured over that inner region only: a card's own rim sits ON the outline and is eroded away, while a cut
    through the underside of the white core is internal and survives. The inner region's edge magnitude is kept
    where the inner mask is and zeroed elsewhere; the inner mask itself rides along as a coverage image. At each
    angle both are rotated, and a row counts only when its covered run is at least half the inner region's width,
    so short rows cannot be the answer. The score is the largest row mean over the inner region's mean: 1 means the
    energy is spread evenly, a tall number means one internal line carries it. Under 1000 inner pixels there is
    nothing to measure and the function refuses, returning 0."""
    inner = ndimage.binary_erosion(near, np.ones((13, 13)))
    if inner.sum() < 1000:
        return 0.0, 0.0
    band = np.where(inner, em, 0.0).astype(np.float32)
    cov = inner.astype(np.float32)
    ys, xs = np.where(inner)
    y0, y1, x0, x1 = ys.min(), ys.max() + 1, xs.min(), xs.max() + 1
    band, cov = band[y0:y1, x0:x1], cov[y0:y1, x0:x1]
    width = x1 - x0
    base = float(band.sum()) / max(1.0, float(cov.sum()))
    if base <= 1e-6:
        return 0.0, 0.0
    best, best_ang = 0.0, 0.0
    for ang in range(-90, 90, 2):
        rb = ndimage.rotate(band, ang, order=1, reshape=True, cval=0.0)
        rc = ndimage.rotate(cov, ang, order=1, reshape=True, cval=0.0)
        run = rc.sum(axis=1)
        keep = run >= 0.5 * width
        if not keep.any():
            continue
        means = rb.sum(axis=1)[keep] / run[keep]
        top = float(means.max()) / base
        if top > best:
            best, best_ang = top, float(ang)
    return best, best_ang


def distinct(jets):
    """Three captures, or one frame counted three times? The fault the owner found: refuse to report numbers this
    evidence cannot have earned."""
    data = []
    for p in jets:
        with open(p, "rb") as f:
            data.append(f.read())
    shas = [hashlib.sha256(d).hexdigest() for d in data]
    imgs = [rgb(p) for p in jets]
    bad = []
    for i in range(3):
        for j in range(i + 1, 3):
            same_sha = shas[i] == shas[j]
            dmax = float(np.abs(imgs[i] - imgs[j]).max()) if imgs[i].shape == imgs[j].shape else 1e9
            if same_sha or dmax <= 0.0:
                bad.append("%s and %s: sha %s, max channel difference %.1f"
                           % (os.path.basename(jets[i]), os.path.basename(jets[j]),
                              "EQUAL" if same_sha else "differ", dmax))
    return shas, bad


def measure(root, label):
    jets, plate_path = paths(root)
    for p in jets + [plate_path]:
        if not os.path.exists(p):
            print("%s: missing %s" % (label, p), file=sys.stderr)
            return None
    shas, bad = distinct(jets)
    print("%s shas: %s" % (label, ", ".join(s[:12] for s in shas)))
    if bad:
        print("%s: one frame counted three times - three captures must be three distinct frames" % label,
              file=sys.stderr)
        for b in bad:
            print("  " + b, file=sys.stderr)
        return None
    plate = rgb(plate_path)
    pe = edges(luma(plate))
    corrs, seams, ratios = [], [], []
    for path in jets:
        jet = rgb(path)
        if jet.shape != plate.shape:
            print("%s and the plate are different sizes" % os.path.basename(path), file=sys.stderr)
            return None
        m = firemask(jet)
        if m.sum() < 15000:
            print("no fire in %s - do not measure this capture" % os.path.basename(path), file=sys.stderr)
            return None
        near, tip, x0, x1, span = near_and_tip(m)
        if near.sum() < 2000 or tip.sum() < 200:
            print("%s: the stream is too small to split" % os.path.basename(path), file=sys.stderr)
            return None
        lum = luma(jet)
        em = edges(lum)
        c = corr(em[near], pe[near])
        s, ang = seam_score(em, near)
        r = float(lum[near].mean()) / max(1e-6, float(lum[tip].mean()))
        corrs.append(c); seams.append(s); ratios.append(r)
        print("%-9s %-14s x %4d..%-4d (%4d px)  near %6d px  tip %5d px  edge_corr %+.4f  seam %.3f @ %+.0f deg"
              "  core/tip %.3f"
              % (label, os.path.basename(path), x0, x1, span, near.sum(), tip.sum(), c, s, ang, r))
    return {"near_edge_corr_each": [round(c, 4) for c in corrs],
            "seam_score_each": [round(s, 4) for s in seams],
            "core_over_tip_luma_each": [round(r, 4) for r in ratios],
            "near_edge_corr_max": round(max(corrs), 4),
            "seam_score_max": round(max(seams), 4),
            "core_over_tip_luma_min": round(min(ratios), 4),
            "shas": shas}


def main(argv):
    only_baseline = "--baseline" in argv[1:]
    base = measure(os.path.join(HERE, "baseline"), "baseline")
    if base is None:
        print("the baseline cannot be measured, so the bars cannot be checked against it", file=sys.stderr)
        return 1
    print("baseline  near_edge_corr %s  min %.4f   (EDGE_BAR %.4f must be BELOW that minimum)"
          % (base["near_edge_corr_each"], min(base["near_edge_corr_each"]), EDGE_BAR))
    print("baseline  seam_score     %s  min %.4f   (SEAM_BAR %.2f must be BELOW that minimum)"
          % (base["seam_score_each"], min(base["seam_score_each"]), SEAM_BAR))
    print("baseline  core_over_tip  %s  min %.4f"
          % (base["core_over_tip_luma_each"], min(base["core_over_tip_luma_each"])))
    bars_separate = EDGE_BAR < min(base["near_edge_corr_each"]) and SEAM_BAR < min(base["seam_score_each"])
    if not bars_separate:
        print("a bar sits inside the current build's own spread: it cannot tell the two builds apart",
              file=sys.stderr)
    out = {"baseline": base, "bars": {"EDGE_BAR": EDGE_BAR, "SEAM_BAR": SEAM_BAR, "LUMA_BAR": LUMA_BAR},
           "bars_outside_baseline_spread": bool(bars_separate)}
    new = None
    if not only_baseline:
        new = measure(HERE, "new")
        out["new"] = new
    if new is not None:
        print("near_edge_corr_max     = %+.4f   (want <= %.4f, the worst of the three)"
              % (new["near_edge_corr_max"], EDGE_BAR))
        print("seam_score_max         = %.3f    (want <= %.2f, the worst of the three; the owner's cut)"
              % (new["seam_score_max"], SEAM_BAR))
        print("core_over_tip_luma_min = %.3f    (want >= %.2f, the lowest of the three)"
              % (new["core_over_tip_luma_min"], LUMA_BAR))
    with open(os.path.join(HERE, "probe.json"), "w", encoding="utf-8") as f:
        json.dump(out, f, indent=2)
        f.write("\n")
    if not bars_separate:
        return 1
    if only_baseline:
        return 0
    if new is None:
        return 1
    ok = (new["near_edge_corr_max"] <= EDGE_BAR and new["seam_score_max"] <= SEAM_BAR
          and new["core_over_tip_luma_min"] >= LUMA_BAR)
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main(sys.argv))
