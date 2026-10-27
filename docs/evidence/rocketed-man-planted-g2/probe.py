#!/usr/bin/env python
"""Does a mask find the planted man's legs, and do they read against the mud? Measured from pixels, not the camera.

The old `rocketed-man-planted/probe.py` scored a 4 px disc placed by camera math alone: `legs_px` worked out to
21.3 on every still (the camera pose never changed between shots) whether or not anything was actually there to
see. That probe could not fail: a flat field of mud scores the same "legs_px" as a still that really shows the man.

This probe uses the geometry (`dist`, `tan_half`, `px_per_m`, `vpm`: CaptureRig.Pose, CaptureRig.cs:167) only to
pick a *search window* around where the legs should be -- never as the measurement. Inside the window it segments
the image (Otsu, no hand threshold), keeps the connected component that looks like a pair of legs (a gate on area,
height, aspect and position), and measures the contrast of THAT mask against a ring of mud around it. No mask that
passes every gate: the still FAILS with the reason, not a number borrowed from the lens.

    dist      = how far the camera is from the point it aims at: the horizontal run over cos(pitch);
    tan(fov/2) = zoom * tan 30 / dist                          (CaptureRig.Pose, CaptureRig.cs:167);
    px_per_m  = h / (2 * dist * tan(fov/2))                    pixels per metre on the focus plane;
    vpm       = px_per_m * cos(cam_pitch)                      pixels per metre of WORLD HEIGHT.
Each still is aimed at ground level under the man (aimY), so the frame centre is his feet-in-the-mud, and his legs
stand DeathGags.PlantLegsAboveM = 0.905 m above it: `legs_px_expected` -- the PRIOR used only to size the window
and the shape gates, never reported as a finding.

Window per still: +-1.2 m either side of the frame centre column, 0.05..1.35 m above the centre row.
Mask gates (all must pass, else FAIL "no mask"):
  - area  >= 0.25 * (0.9 m x 0.35 m in px)
  - bbox height >= 0.5 * legs_px_expected
  - bbox height > bbox width
  - mask centroid within 0.4 m of the centre column
contrast = |median luma of the mask pixels - median luma of the ring 1.3..2.3 * legs_px_measured around the mask
centroid (mask pixels excluded)| / (ring + 0.02)   -- same ratio shape as CaptureRig's, so the numbers compare.

BAR = 0.10 (docs/reference/workflow.md section 6: on this scale 0.4 means you can find the man, 0.1 means he is
mud). The probe also fails if the bar sits inside the spread of the three captures' own contrast values -- a pass
would then mean nothing more than noise.
"""
import json, math, os, statistics, sys

from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
LEGS_M = 0.905          # DeathGags.PlantLegsAboveM: the owner's pick A, waist deep
LEG_W_M, LEG_H_M = 0.35, 0.9    # a pair of legs' rough footprint, for the area gate
WIN_HALF_X_M = 1.2
WIN_TOP_M, WIN_BOTTOM_M = 1.35, 0.05     # above the centre row
BAR = 0.10               # docs/reference/workflow.md section 6: 0.4 finds him, 0.1 is mud


def luma(px):
    r, g, b = px[0] / 255.0, px[1] / 255.0, px[2] / 255.0
    return 0.2126 * r + 0.7152 * g + 0.0722 * b


def geometry(s, h):
    run = math.hypot(s["cam_x"] - s["focus_x"], s["cam_z"] - s["focus_z"])
    pitch = math.radians(s["cam_pitch"])
    dist = run / math.cos(pitch)
    tan_half = s["zoom"] * math.tan(math.radians(30.0)) / dist
    px_per_m = h / (2.0 * dist * tan_half)
    vpm = px_per_m * math.cos(pitch)
    return dist, px_per_m, vpm


def otsu(values):
    """256-bin Otsu threshold (0-1 luma values). Returns a split value t: class A is < t, class B is >= t."""
    hist = [0] * 256
    for v in values:
        hist[min(255, max(0, int(v * 255)))] += 1
    total = len(values)
    sum_all = sum(i * hist[i] for i in range(256))
    sum_b, w_b, best_k, best_var = 0.0, 0, 0, -1.0
    for k in range(256):
        w_b += hist[k]
        if w_b == 0:
            continue
        w_f = total - w_b
        if w_f == 0:
            break
        sum_b += k * hist[k]
        m_b = sum_b / w_b
        m_f = (sum_all - sum_b) / w_f
        var_between = w_b * w_f * (m_b - m_f) ** 2
        if var_between > best_var:
            best_var, best_k = var_between, k
    # bin best_k's upper edge: a value that rounds into bin best_k must stay in the "below" class
    return (best_k + 1) / 255.0


def components(mask, w, h, x0, y0):
    """4-neighbour flood fill of `mask` (set of (x, y) local coords). Returns a list of sets of (x, y) local coords."""
    seen = set()
    out = []
    for p in mask:
        if p in seen:
            continue
        stack, comp = [p], set()
        while stack:
            cx, cy = stack.pop()
            if (cx, cy) in seen or (cx, cy) not in mask:
                continue
            seen.add((cx, cy))
            comp.add((cx, cy))
            stack += [(cx + 1, cy), (cx - 1, cy), (cx, cy + 1), (cx, cy - 1)]
        out.append(comp)
    return out


def ring_median(lum, w, h, cx, cy, r_in, r_out, exclude):
    r_out = int(math.ceil(r_out))
    vals = []
    for y in range(max(0, cy - r_out), min(h, cy + r_out + 1)):
        for x in range(max(0, cx - r_out), min(w, cx + r_out + 1)):
            if (x, y) in exclude:
                continue
            d = math.hypot(x - cx, y - cy)
            if r_in <= d <= r_out:
                vals.append(lum[y * w + x])
    return statistics.median(vals) if vals else None


def find_mask(lum, w, h, cx, cy, px_per_m, vpm, legs_px_expected):
    x0 = max(0, int(round(cx - WIN_HALF_X_M * px_per_m)))
    x1 = min(w, int(round(cx + WIN_HALF_X_M * px_per_m)))
    y0 = max(0, int(round(cy - WIN_TOP_M * vpm)))
    y1 = min(h, int(round(cy - WIN_BOTTOM_M * vpm)))
    win = {"x0": x0, "x1": x1, "y0": y0, "y1": y1}
    if x1 <= x0 or y1 <= y0:
        return None, win, "window empty"

    values = [lum[y * w + x] for y in range(y0, y1) for x in range(x0, x1)]
    if max(values) - min(values) < 1e-6:
        return None, win, "no mask"
    t = otsu(values)
    below = {(x - x0, y - y0) for y in range(y0, y1) for x in range(x0, x1) if lum[y * w + x] < t}
    above = {(x - x0, y - y0) for y in range(y0, y1) for x in range(x0, x1) if lum[y * w + x] >= t}
    # the legs could read darker or lighter than the mud depending on the light: look at both Otsu classes and let
    # the gates (shape, size, position), not a guess about which side is "the object", decide which component is legs
    cands = []
    for cls in (below, above):
        if not cls:
            continue
        for comp in components(cls, x1 - x0, y1 - y0, x0, y0):
            xs = [p[0] for p in comp]
            ys = [p[1] for p in comp]
            bx0, bx1, by0, by1 = min(xs), max(xs), min(ys), max(ys)
            bb_w, bb_h = bx1 - bx0 + 1, by1 - by0 + 1
            ccx = x0 + statistics.mean(xs)
            ccy = y0 + statistics.mean(ys)
            cands.append({"comp": comp, "bb_w": bb_w, "bb_h": bb_h, "area": len(comp), "cx": ccx, "cy": ccy})
    if not cands:
        return None, win, "no mask"

    area_gate = 0.25 * (LEG_W_M * px_per_m) * (LEG_H_M * vpm)

    def gate(c):
        if c["area"] < area_gate:
            return "area %d < gate %d" % (c["area"], round(area_gate))
        if c["bb_h"] < 0.5 * legs_px_expected:
            return "bbox height %d < gate %d" % (c["bb_h"], round(0.5 * legs_px_expected))
        if c["bb_h"] <= c["bb_w"]:
            return "bbox height %d not > width %d" % (c["bb_h"], c["bb_w"])
        if abs(c["cx"] - cx) > 0.4 * px_per_m:
            return "centroid %.1f px off, gate %.1f" % (abs(c["cx"] - cx), 0.4 * px_per_m)
        return None

    cands.sort(key=lambda c: abs(c["cx"] - cx))
    passing = [c for c in cands if gate(c) is None]
    if passing:
        passing.sort(key=lambda c: abs(c["cx"] - cx))
        return passing[0], win, None
    best = cands[0]
    return best, win, gate(best)


def probe(png, write_mask):
    side = os.path.splitext(png)[0] + ".json"
    s = json.load(open(side, encoding="utf-8"))
    img = Image.open(png).convert("RGB")
    w, h = img.size
    pixels = list(img.getdata())
    lum = [luma(p) for p in pixels]

    dist, px_per_m, vpm = geometry(s, h)
    legs_px_expected = LEGS_M * vpm
    cx, cy = w // 2, h // 2

    mask, win, reason = find_mask(lum, w, h, cx, cy, px_per_m, vpm, legs_px_expected)
    base = {
        "still": os.path.basename(png), "legs_px_expected": round(legs_px_expected, 1),
        "px_per_m": round(px_per_m, 2), "vpm": round(vpm, 2), "window": win,
    }

    if reason is not None:
        base.update({"ok": False, "reason": reason, "contrast": None})
        if write_mask:
            draw_mask(img, win, mask["comp"] if mask else set(), win["x0"], win["y0"],
                      os.path.join(HERE, "mask_" + base["still"]), False)
        return base

    xs = [win["x0"] + p[0] for p in mask["comp"]]
    ys = [win["y0"] + p[1] for p in mask["comp"]]
    world = set(zip(xs, ys))
    mask_vals = [lum[y * w + x] for (x, y) in world]
    man = statistics.median(mask_vals)
    legs_px_measured = mask["bb_h"]
    bg = ring_median(lum, w, h, int(round(mask["cx"])), int(round(mask["cy"])),
                      1.3 * legs_px_measured, 2.3 * legs_px_measured, world)
    contrast = abs(man - bg) / (bg + 0.02)

    base.update({
        "ok": True, "reason": None, "contrast": round(contrast, 5),
        "legs_px_measured": legs_px_measured, "mask_w_px": mask["bb_w"], "mask_area_px": mask["area"],
        "mask_cx": round(mask["cx"], 1), "mask_cy": round(mask["cy"], 1),
        "mask_luma": round(man, 5), "ring_luma": round(bg, 5),
    })
    if write_mask:
        draw_mask(img, win, mask["comp"], win["x0"], win["y0"],
                  os.path.join(HERE, "mask_" + base["still"]), True)
    return base


def draw_mask(img, win, comp_local, x0, y0, out_path, found):
    out = img.copy()
    px = out.load()
    w, h = out.size
    for (lx, ly) in comp_local:
        x, y = x0 + lx, y0 + ly
        if 0 <= x < w and 0 <= y < h:
            px[x, y] = (255, 0, 0)
    for x in range(win["x0"], win["x1"]):
        for y in (win["y0"], win["y1"] - 1):
            if 0 <= x < w and 0 <= y < h:
                px[x, y] = (0, 255, 0) if found else (255, 255, 0)
    for y in range(win["y0"], win["y1"]):
        for x in (win["x0"], win["x1"] - 1):
            if 0 <= x < w and 0 <= y < h:
                px[x, y] = (0, 255, 0) if found else (255, 255, 0)
    out.save(out_path)


def selftest():
    def make(w, h, bg_luma, patch=None, patch_luma=None):
        bg8 = int(round(bg_luma * 255))
        img = Image.new("RGB", (w, h), (bg8, bg8, bg8))
        if patch:
            px0, py0, pw, ph = patch
            p8 = int(round(patch_luma * 255))
            for y in range(py0, py0 + ph):
                for x in range(px0, px0 + pw):
                    img.putpixel((x, y), (p8, p8, p8))
        return img

    # a still 300x300, centre 150,150; PPM (px per metre, both axes since pitch=0) chosen so the window comfortably
    # holds an 8x21 px patch and the gates scale with it.
    W = H = 300
    PPM = 20.0
    cases = []

    def sidecar_for(ppm):
        dist = 100.0
        zoom = H / (2.0 * ppm * math.tan(math.radians(30.0)))
        return {"cam_x": 0.0, "cam_z": -dist, "focus_x": 0.0, "focus_z": 0.0, "cam_pitch": 0.0, "zoom": zoom}

    sc = sidecar_for(PPM)
    cy = H // 2
    win_y0 = cy - int(round(WIN_TOP_M * PPM))

    # (a) an 8x21 px legs-shaped patch at luma 0.30 on a 0.18 mud field -> contrast 0.600, PASS
    patch_w, patch_h = 8, 21
    px0 = W // 2 - patch_w // 2
    py0 = win_y0 + 2
    img_a = make(W, H, 0.18, (px0, py0, patch_w, patch_h), 0.30)
    cases.append(("pass", img_a, sc, 0.18, 0.30))

    # (b) the same patch at 0.186 -> contrast 0.030, FAIL under the bar
    img_b = make(W, H, 0.18, (px0, py0, patch_w, patch_h), 0.186)
    cases.append(("fail-bar", img_b, sc, 0.18, 0.186))

    # (c) a flat 0.18 field -> FAIL no mask
    img_c = make(W, H, 0.18)
    cases.append(("fail-no-mask", img_c, sc, 0.18, None))

    # (d) a 30x6 px bar (a sandbag shape: wider than tall) -> FAIL on the shape gate
    img_d = make(W, H, 0.18, (W // 2 - 15, py0, 30, 6), 0.30)
    cases.append(("fail-shape", img_d, sc, 0.18, None))

    ok = True
    for name, img, sc_data, bg_l, patch_l in cases:
        w, h = img.size
        lum = [luma(p) for p in img.getdata()]
        dist, px_per_m, vpm = geometry(sc_data, h)
        cx, cy = w // 2, h // 2
        legs_exp = LEGS_M * vpm
        mask, win, reason = find_mask(lum, w, h, cx, cy, px_per_m, vpm, legs_exp)
        if name == "pass":
            expect = round(abs(patch_l - bg_l) / (bg_l + 0.02), 3)
            got_fail = reason is not None
            if got_fail:
                print("SELFTEST %-14s FAIL  expected PASS (reason=%s)" % (name, reason)); ok = False
                continue
            xs = [win["x0"] + p[0] for p in mask["comp"]]
            ys = [win["y0"] + p[1] for p in mask["comp"]]
            world = set(zip(xs, ys))
            man = statistics.median(lum[y * w + x] for (x, y) in world)
            bgm = ring_median(lum, w, h, int(round(mask["cx"])), int(round(mask["cy"])),
                               1.3 * mask["bb_h"], 2.3 * mask["bb_h"], world)
            contrast = round(abs(man - bgm) / (bgm + 0.02), 3)
            good = contrast >= BAR and abs(contrast - expect) < 0.03
            print("SELFTEST %-14s %-4s contrast %.3f expected ~%.3f" % (name, "PASS" if good else "FAIL", contrast, expect))
            ok = ok and good
        elif name == "fail-bar":
            if reason is not None:
                print("SELFTEST %-14s FAIL  expected a mask under the bar (reason=%s)" % (name, reason)); ok = False
                continue
            xs = [win["x0"] + p[0] for p in mask["comp"]]
            ys = [win["y0"] + p[1] for p in mask["comp"]]
            world = set(zip(xs, ys))
            man = statistics.median(lum[y * w + x] for (x, y) in world)
            bgm = ring_median(lum, w, h, int(round(mask["cx"])), int(round(mask["cy"])),
                               1.3 * mask["bb_h"], 2.3 * mask["bb_h"], world)
            contrast = round(abs(man - bgm) / (bgm + 0.02), 3)
            good = contrast < BAR
            print("SELFTEST %-14s %-4s contrast %.3f < bar %.3f" % (name, "PASS" if good else "FAIL", contrast, BAR))
            ok = ok and good
        else:
            good = reason is not None and (("no mask" in reason) if name == "fail-no-mask" else ("width" in reason or "height" in reason))
            print("SELFTEST %-14s %-4s reason=%s" % (name, "PASS" if good else "FAIL", reason))
            ok = ok and good
    return 0 if ok else 1


def main():
    argv = sys.argv[1:]
    if "--selftest" in argv:
        return selftest()

    write_mask = "--no-write" not in argv
    directory = HERE
    if "--dir" in argv:
        directory = argv[argv.index("--dir") + 1]

    stills = sorted(f for f in os.listdir(directory) if f.startswith("plant_") and f.endswith(".png"))
    if not stills:
        print("no plant_*.png in " + directory, file=sys.stderr)
        return 1
    old_here = globals()["HERE"]
    try:
        globals()["HERE"] = directory
        samples = [probe(os.path.join(directory, f), write_mask) for f in stills]
    finally:
        globals()["HERE"] = old_here

    for s in samples:
        if s["ok"]:
            print("%s mask %dx%d px  luma %.4f  ring %.4f  contrast %.4f" %
                  (s["still"], s["mask_w_px"], s["legs_px_measured"], s["mask_luma"], s["ring_luma"], s["contrast"]))
        else:
            print("%s FAIL %s" % (s["still"], s["reason"]))

    oks = [s for s in samples if s["ok"]]
    any_fail = len(oks) != len(samples)
    contrasts = [s["contrast"] for s in oks]
    lo = min(contrasts) if contrasts else 0.0
    hi = max(contrasts) if contrasts else 0.0
    margin = lo - BAR
    spread = hi - lo
    under_bar = any(c < BAR for c in contrasts)
    bar_in_spread = margin <= spread

    out = {"bar": BAR, "samples": samples, "lo": round(lo, 5), "hi": round(hi, 5),
           "margin": round(margin, 5), "spread": round(spread, 5),
           "pass": (not any_fail) and (not under_bar) and (not bar_in_spread)}
    if write_mask:
        with open(os.path.join(directory, "probe.json"), "w", encoding="utf-8") as fh:
            json.dump(out, fh, indent=2)
            fh.write("\n")

    print("bar %.3f  three captures %.3f..%.3f  margin %.3f  spread %.3f" % (BAR, lo, hi, margin, spread))

    if any_fail:
        print("at least one still has no mask", file=sys.stderr)
        return 1
    if under_bar:
        print("at least one still is under the bar", file=sys.stderr)
        return 1
    if bar_in_spread:
        print("the bar sits inside the captures' own spread", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
