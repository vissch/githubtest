"""Measures the Sapper card's portrait as the card actually shows it: HudCapture.Shoot's committed stills of
the real BattleHud (GreyboxCorridor/playtest level, the Sapper put in slot 0 of the Iron ten so his card is on
screen - the default ten never fields him, SimWorld.FillRosters refuses a unit the faction doesn't own outside
a test level). Three captures (card-base-{1,2,3}.png + their .png.json sidecars) prove the measure is stable
before any repaint; part 2 adds card-fix-{1,2,3}.png from the repainted portrait and raises the bar.

CROP: the sidecar's "portrait-Sapper" rect is the UI element's full worldBound in capture pixels, but the
portrait image itself is drawn `background-size: contain` inside it (dustfront.components.uss:137) from a
square 256x256 source, so the picture is the centred square of side min(w, h) within that rect - cropping the
full rect would include empty card plate on whichever axis is wider.

Numbers are read from pixels, never typed. Part 1 of the plan: only the three base captures are compared to
each other here (both metrics' spread, and zero drift on the untouched Engineer card as a control for capture
noise); the part-2 bars against the fix come later, once card-fix-*.png exists.

Part 2 adds card-fix-{1,2,3}.png (the repainted PaintSapperPortrait, same scene, same slot) and checks both
metrics clear a bar set from the base spread: each bar is the larger of 2x the base max and a design floor
(0.10 for kit_share, 0.30 for figure_cover), and every fix capture's value must clear it. engineer_drift_px
between card-base-1 and card-fix-1 must stay 0: the repaint touched only the Sapper portrait, nothing else on
the card captured in the same still.
"""
import json
import os

from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))

PACK = (0xA8, 0x99, 0x70)
CHARGE = (0x6E, 0x62, 0x45)
DISC = (0x23, 0x25, 0x2A)
KIT_THRESHOLD = 24
LUMA_THRESHOLD = 24


def portrait_square(png_path, json_path, name):
    with open(json_path, "r", encoding="utf-8") as f:
        sidecar = json.load(f)
    x, y, w, h = sidecar["rects"][name]
    side = min(w, h)
    cx, cy = x + w / 2.0, y + h / 2.0
    x0, y0 = cx - side / 2.0, cy - side / 2.0
    img = Image.open(png_path).convert("RGB")
    return img.crop((round(x0), round(y0), round(x0 + side), round(y0 + side)))


def luma(r, g, b):
    return 0.299 * r + 0.587 * g + 0.114 * b


def kit_share(square):
    px = square.load()
    w, h = square.size
    hits = 0
    for y in range(h):
        for x in range(w):
            r, g, b = px[x, y]
            dp = max(abs(r - PACK[0]), abs(g - PACK[1]), abs(b - PACK[2]))
            dc = max(abs(r - CHARGE[0]), abs(g - CHARGE[1]), abs(b - CHARGE[2]))
            if dp <= KIT_THRESHOLD or dc <= KIT_THRESHOLD:
                hits += 1
    return hits / float(w * h)


def figure_cover(square):
    px = square.load()
    w, h = square.size
    disc_luma = luma(*DISC)
    y0 = h // 2
    hits = 0
    total = w * (h - y0)
    for y in range(y0, h):
        for x in range(w):
            r, g, b = px[x, y]
            if luma(r, g, b) - disc_luma >= LUMA_THRESHOLD:
                hits += 1
    return hits / float(total) if total > 0 else 0.0


def diff_px(a, b, threshold=10):
    pa = a.load()
    pb = b.load()
    w, h = a.size
    hits = 0
    for y in range(h):
        for x in range(w):
            ra, ga, ba = pa[x, y]
            rb, gb, bb = pb[x, y]
            d = max(abs(ra - rb), abs(ga - gb), abs(ba - bb))
            if d > threshold:
                hits += 1
    return hits


def main():
    squares = {}
    engineer_squares = {}
    for i in (1, 2, 3):
        png = os.path.join(HERE, f"card-base-{i}.png")
        js = png + ".json"
        squares[i] = portrait_square(png, js, "portrait-Sapper")
        engineer_squares[i] = portrait_square(png, js, "portrait-Engineer")

    kit = {i: kit_share(squares[i]) for i in (1, 2, 3)}
    cover = {i: figure_cover(squares[i]) for i in (1, 2, 3)}

    kit_vals = list(kit.values())
    cover_vals = list(cover.values())
    kit_spread = max(kit_vals) - min(kit_vals)
    cover_spread = max(cover_vals) - min(cover_vals)

    pairs = [(1, 2), (1, 3), (2, 3)]
    engineer_drift = {f"{a}-{b}": diff_px(engineer_squares[a], engineer_squares[b]) for a, b in pairs}

    fix_squares = {i: portrait_square(os.path.join(HERE, f"card-fix-{i}.png"), os.path.join(HERE, f"card-fix-{i}.png.json"), "portrait-Sapper") for i in (1, 2, 3)}
    kit_fix = {i: kit_share(fix_squares[i]) for i in (1, 2, 3)}
    cover_fix = {i: figure_cover(fix_squares[i]) for i in (1, 2, 3)}

    base_fix_drift = diff_px(
        portrait_square(os.path.join(HERE, "card-base-1.png"), os.path.join(HERE, "card-base-1.png.json"), "portrait-Engineer"),
        portrait_square(os.path.join(HERE, "card-fix-1.png"), os.path.join(HERE, "card-fix-1.png.json"), "portrait-Engineer"))

    probe = {
        "kit_share": kit,
        "kit_share_base_max": max(kit_vals),
        "kit_share_base_spread": kit_spread,
        "kit_share_fix": kit_fix,
        "figure_cover": cover,
        "figure_cover_base_max": max(cover_vals),
        "figure_cover_base_spread": cover_spread,
        "figure_cover_fix": cover_fix,
        "engineer_drift_px": engineer_drift,
        "engineer_drift_px_base_vs_fix": base_fix_drift,
    }
    with open(os.path.join(HERE, "probe.json"), "w", encoding="utf-8") as f:
        json.dump(probe, f, indent=2)
    print(json.dumps(probe, indent=2))

    failed = False
    if not (kit_spread <= 0.005):
        print(f"FAIL kit_share_base_spread: {kit_spread} > 0.005")
        failed = True
    if not (cover_spread <= 0.005):
        print(f"FAIL figure_cover_base_spread: {cover_spread} > 0.005")
        failed = True
    for pair, drift in engineer_drift.items():
        if drift != 0:
            print(f"FAIL engineer_drift_px[{pair}]: {drift} != 0")
            failed = True
    if base_fix_drift != 0:
        print(f"FAIL engineer_drift_px_base_vs_fix: {base_fix_drift} != 0")
        failed = True

    bars = {
        "kit_share": max(2 * probe["kit_share_base_max"], 0.10),
        "figure_cover": max(2 * probe["figure_cover_base_max"], 0.30),
    }
    fix_vals = {"kit_share": kit_fix, "figure_cover": cover_fix}
    for key, bar in bars.items():
        base = probe[key + "_base_max"]
        worst = min(fix_vals[key].values())
        print(f"{key}: base_max={base} bar={bar} fix_min={worst}")
        if not (base < bar <= worst):
            print(f"FAIL {key}: base_max={base} bar={bar} fix_min={worst}")
            failed = True

    return 1 if failed else 0


if __name__ == "__main__":
    raise SystemExit(main())
