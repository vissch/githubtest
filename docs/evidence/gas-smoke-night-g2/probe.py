# Phase: gas-smoke-night-g2 (the owner, 2026-10-20). Started from docs/evidence/gas-smoke-night/probe.py and keeping
# its luma exactly: Rec.709 on 0-1, CaptureRig's own scale (0.2126 R + 0.7152 G + 0.0722 B, bytes / 255), never mixed
# with shotstats.py's 0-255.
#
# What changed, and why:
#   * No `min` of gas and smoke anywhere. g1 published the min of the two stills per field, so smoke's empty day frame
#     hid the gas ring the owner could see. Every still keeps its own numbers and its own bar.
#   * The box is AIMED, not the centre of the frame: each still's <stem>.aim.json (written by TW.Tests.GasNightStills
#     from the sim field and the still's own camera pose) says where the cloud stands, where the ground strips are,
#     where its far ground edge is, and how many pixels a metre is. A fixed centre box landed half on the wreck that
#     stands in the gas frame.
#   * Gas is scored on two more numbers, because "a flat yellow stain" and "a cloud with a rim and some height" have
#     the same contrast:
#       rim      — Michelson of the cloud mask's top 6 rows per column against the mask's middle rows. A flat stain
#                  scores about 0; a lit lip on top of the bank scores high.
#       height_m — the mask rows standing ABOVE the cloud's own far ground edge, in metres (px_per_m). A stain painted
#                  on the ground has none of its mask above its own far edge, so this is height a player sees at zoom
#                  30, not screen spread.
#
# Bars live in bars.json beside this file; the three takes they were set from live in baseline.json. Prints every
# number against its bar and exits 1 on the first miss. `--baseline` prints the three takes instead and exits 0 when
# each still has a spread written. Needs Pillow.
import json
import os
import statistics
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
STILLS = ("gas_night", "gas_day", "smoke_night", "smoke_day")
MASK_CONTRAST = 0.12   # a box pixel belongs to the cloud at or above this contrast against the ground median
RIM_ROWS = 6           # the mask's top rows per column that make the lip
FAR_SPAN = 0.05        # the columns "around far_edge": this fraction of the frame width either side

DEFINITION = {
    "luma": "Rec.709 on 0-1, CaptureRig's scale (0.2126 R + 0.7152 G + 0.0722 B, bytes / 255)",
    "ground": "median luma of ground_l and ground_r from the still's aim.json (the left and right tenth over the box's rows)",
    "contrast": "median over the AIMED box of |Lc - Lg| / (Lc + Lg)",
    "mask": "box pixels with contrast >= %.2f against the ground median" % MASK_CONTRAST,
    "rim": "Michelson of the mask's top %d rows per column against the mask's middle rows" % RIM_ROWS,
    "height_m": "median over the columns within %.2f of the frame width of far_edge of the mask rows ABOVE far_edge, / px_per_m" % FAR_SPAN,
    "note": "no min of gas and smoke: every still keeps its own numbers and its own bar",
}


def load_luma(path):
    """The still as a list of rows of luma, 0-1, y counted from the top as Pillow counts rows."""
    from PIL import Image
    im = Image.open(path).convert("RGB")
    w, h = im.size
    px = im.load()
    rows = []
    for y in range(h):
        row = [0.0] * w
        for x in range(w):
            r, g, b = px[x, y]
            row[x] = (0.2126 * r + 0.7152 * g + 0.0722 * b) / 255.0
        rows.append(row)
    return rows, w, h


def box_lumas(rows, box, w, h):
    x0, y0, x1, y1 = box
    x0, x1 = max(0, x0), min(w, x1)
    y0, y1 = max(0, y0), min(h, y1)
    out = []
    for y in range(y0, y1):
        row = rows[y]
        out.extend(row[x0:x1])
    return out


def michelson(a, b):
    return abs(a - b) / (a + b) if (a + b) > 0 else 0.0


def measure(stem, where=None):
    """Every number this still carries, from its png and its aim.json. `where` films from another folder (the baseline's
    three takes live outside the checkout), `stem` still decides whether it is scored as gas."""
    base = where or os.path.join(HERE, stem)
    png = base + ".png"
    aim_path = base + ".aim.json"
    with open(aim_path, encoding="utf-8") as f:
        aim = json.load(f)
    rows, w, h = load_luma(png)
    ground = statistics.median(box_lumas(rows, aim["ground_l"], w, h) + box_lumas(rows, aim["ground_r"], w, h))
    box = aim["box"]
    cloud = box_lumas(rows, box, w, h)
    out = {
        "contrast": round(statistics.median(michelson(lc, ground) for lc in cloud), 5),
        "ground_median": round(ground, 5),
        "cloud_median": round(statistics.median(cloud), 5),
        "box": box,
    }
    if not os.path.basename(stem).startswith("gas"):
        return out

    # The cloud mask, per column: which rows belong to the cloud. Taken over mask_box, the aimed box with its rows
    # stretched to take the cloud's far ground edge in — that edge projects above the box's top row (farther away is
    # higher up the picture), so a mask cut to the box would have nothing above it and height_m would read 0 for any
    # cloud. contrast keeps the plain box.
    x0, y0, x1, y1 = aim.get("mask_box", box)
    cols = {}
    for x in range(max(0, x0), min(w, x1)):
        ys = [y for y in range(max(0, y0), min(h, y1)) if michelson(rows[y][x], ground) >= MASK_CONTRAST]
        if ys:
            cols[x] = ys

    # rim: the lip on top of the bank against its own body
    top, mid = [], []
    for x, ys in cols.items():
        ys = sorted(ys)
        top.extend(rows[y][x] for y in ys[:RIM_ROWS])
        body = ys[RIM_ROWS:]
        if len(body) >= 3:
            lo = len(body) // 4
            mid.extend(rows[y][x] for y in body[lo:len(body) - lo] or body)
    out["mask_columns"] = len(cols)
    out["rim"] = round(michelson(statistics.median(top), statistics.median(mid)), 5) if top and mid else 0.0

    # height_m: what stands above the cloud's own far ground edge, in metres
    fx, fy = aim["far_edge"]
    span = w * FAR_SPAN
    heights = []
    for x, ys in cols.items():
        if abs(x - fx) > span:
            continue
        heights.append(sum(1 for y in ys if y < fy))
    ppm = aim["px_per_m"]
    out["height_px"] = round(statistics.median(heights), 2) if heights else 0.0
    out["px_per_m"] = ppm
    out["height_m"] = round(out["height_px"] / ppm, 4) if ppm > 0 else 0.0
    return out


def baseline():
    """The three takes the bars were set from, and whether each still has its spread written."""
    path = os.path.join(HERE, "baseline.json")
    if not os.path.exists(path):
        print("no baseline.json beside the probe")
        return 1
    with open(path, encoding="utf-8") as f:
        base = json.load(f)
    print("baseline, commit filmed %s" % base.get("commit", "(unsaid)"))
    missing = []
    for stem in STILLS:
        per = base.get("per_still", {}).get(stem)
        if not per:
            missing.append(stem + ": no takes")
            continue
        for name, rec in sorted(per.items()):
            takes = rec.get("takes")
            if not takes or len(takes) != 3 or "spread" not in rec:
                missing.append("%s %s: needs three takes and a spread" % (stem, name))
                continue
            print("  %-11s %-9s takes %s  min %.4f  max %.4f  spread %.4f"
                  % (stem, name, " ".join("%.4f" % t for t in takes), rec["min"], rec["max"], rec["spread"]))
    for m in missing:
        print("MISSING " + m)
    return 1 if missing else 0


def main():
    if "--baseline" in sys.argv:
        return baseline()
    with open(os.path.join(HERE, "bars.json"), encoding="utf-8") as f:
        bars = json.load(f)
    out = {"definition": DEFINITION, "per_still": {}}
    miss = None
    for stem in STILLS:
        m = measure(stem)
        out["per_still"][stem] = m
        for name, bar in sorted(bars.get("per_still", {}).get(stem, {}).items()):
            got = m.get(name)
            lo, hi = bar.get("min"), bar.get("max")
            ok = got is not None and (lo is None or got >= lo) and (hi is None or got <= hi)
            want = (">= %.4f" % lo) if hi is None else ("<= %.4f" % hi) if lo is None else ("%.4f-%.4f" % (lo, hi))
            print("%-11s %-9s %8.4f  bar %-16s %s" % (stem, name, got if got is not None else float("nan"), want,
                                                      "ok" if ok else "MISS"))
            if not ok and miss is None:
                miss = "%s %s %s, bar %s" % (stem, name, "none" if got is None else "%.4f" % got, want)
        out["per_still"][stem]["bars"] = bars.get("per_still", {}).get(stem, {})
    with open(os.path.join(HERE, "probe.json"), "w", encoding="utf-8") as f:
        json.dump(out, f, indent=2, sort_keys=True)
        f.write("\n")
    if miss:
        print("MISS: " + miss)
        return 1
    print("every still over its bar")
    return 0


if __name__ == "__main__":
    sys.exit(main())
