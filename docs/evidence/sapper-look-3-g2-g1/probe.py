"""Measures whether the Sapper's card portrait reads as a man - a helmet over a face, shoulders, a pack and two
separate charges - instead of the grey blob over a tan flood the g2 round left (the owner, 2026-10-25: "a grey
helmet blob over a tan flood, not a shoulders-up sapper with a pack and charges").

Why a new probe: g2's kit_share/figure_cover only asked "is there kit tone, is the lower disc lighter than the
plate", which a flood of pack tone passes. These metrics ask for PARTS, counted on the pixels:
  face_share        face tone / the crop           - there must be a face at all
  pack_parts        pack components >= 0.004       - torso AND pack, not one slab
  charge_parts      charge components >= 0.004     - exactly two charges, separate
  pack_flood        largest pack part / lower half - the flood itself, less is better
  helmet_above_face helmet centroid row above the face's by 0.05 x side - shoulders-up order

Captures: HudCapture.Shoot stills of the real BattleHud (GreyboxCorridor with SimHost.GeneratedBattlefield=false
and PlaytestMap=true, which turns Init.TestLevel on; the Sapper written into slot 0 of the Iron ten through
SimHost.WriteWorlds, then the HudController toggled off and on so HudView.Build makes his card - the cards are
built once from the roster, HudController.cs:132). cur-{1,2,3}.png are the current build at 1280x720, 1920x1080
and 2560x1440: the HUD panel is ScaleWithScreenSize (HudLayout.cs:15), so the card rasterises at three different
sizes - three real captures, not one picture copied three times. fix-{1,2,3}.png are the repainted portrait, same
scene, same slot, same three sizes.

CROP: the sidecar's "portrait-Sapper" rect is the element's worldBound in capture pixels, but the image is drawn
`background-size: contain` from a square source (dustfront.components.uss:137), so the picture is the centred
square of side min(w, h) in that rect.

Numbers are read from pixels, never typed. `--base` measures only cur-*.png and asserts the three crops are
different sizes, the metrics agree across them (counts to within one part: see the note in main), and the current
build misses the part checks - no face tone at all, and one slab of pack tone over half the lower disc. With no argument it
also measures fix-*.png and every bar, each bar set off the cur-* measurements or the owner's words.
"""
import json
import os
import sys

from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))

TONES = {
    "HELMET": (0x3E, 0x4A, 0x52),
    "FACE": (0xD8, 0xA0, 0x6A),
    "PACK": (0xA8, 0x99, 0x70),     # sapperPackMat
    "CHARGE": (0x6E, 0x62, 0x45),   # sapperChargeMat
    "DISC": (0x23, 0x25, 0x2A),
}
TONE_THRESHOLD = 32
MIN_PART = 0.004      # of the square, for a component to count as a part
SPREAD = 0.05         # allowed drift of a share metric over the three captures


def portrait_square(png_path, json_path, name):
    with open(json_path, "r", encoding="utf-8") as f:
        sidecar = json.load(f)
    x, y, w, h = sidecar["rects"][name]
    side = min(w, h)
    cx, cy = x + w / 2.0, y + h / 2.0
    x0, y0 = cx - side / 2.0, cy - side / 2.0
    img = Image.open(png_path).convert("RGB")
    return img.crop((round(x0), round(y0), round(x0 + side), round(y0 + side)))


def diff_px(a, b, threshold=10):
    pa, pb = a.load(), b.load()
    w, h = a.size
    hits = 0
    for y in range(h):
        for x in range(w):
            ra, ga, ba = pa[x, y]
            rb, gb, bb = pb[x, y]
            if max(abs(ra - rb), abs(ga - gb), abs(ba - bb)) > threshold:
                hits += 1
    return hits


def classify(square):
    """A label grid: the nearest declared tone by max-channel distance, None past TONE_THRESHOLD."""
    px = square.load()
    w, h = square.size
    grid = [[None] * w for _ in range(h)]
    for y in range(h):
        row = grid[y]
        for x in range(w):
            r, g, b = px[x, y]
            best, bestd = None, TONE_THRESHOLD
            for name, (tr, tg, tb) in TONES.items():
                d = max(abs(r - tr), abs(g - tg), abs(b - tb))
                if d <= bestd:
                    best, bestd = name, d
            row[x] = best
    return grid, w, h


def components(grid, w, h, label):
    """Connected (4-neighbour) runs of one label, biggest first, as pixel counts."""
    seen = [[False] * w for _ in range(h)]
    out = []
    for y0 in range(h):
        for x0 in range(w):
            if seen[y0][x0] or grid[y0][x0] != label:
                continue
            stack = [(x0, y0)]
            seen[y0][x0] = True
            area = 0
            while stack:
                x, y = stack.pop()
                area += 1
                for nx, ny in ((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)):
                    if 0 <= nx < w and 0 <= ny < h and not seen[ny][nx] and grid[ny][nx] == label:
                        seen[ny][nx] = True
                        stack.append((nx, ny))
            out.append(area)
    out.sort(reverse=True)
    return out


def centroid_row(grid, w, h, label):
    total, rows = 0, 0
    for y in range(h):
        row = grid[y]
        for x in range(w):
            if row[x] == label:
                total += 1
                rows += y
    return (rows / float(total)) if total else None


def measure(square):
    grid, w, h = classify(square)
    area = float(w * h)
    floor = MIN_PART * area
    pack = components(grid, w, h, "PACK")
    charge = components(grid, w, h, "CHARGE")
    face = sum(1 for y in range(h) for x in range(w) if grid[y][x] == "FACE")
    lower = float(w * (h - h // 2))
    helmet_row = centroid_row(grid, w, h, "HELMET")
    face_row = centroid_row(grid, w, h, "FACE")
    return {
        "side": w,
        "face_share": face / area,
        "pack_parts": sum(1 for a in pack if a >= floor),
        "charge_parts": sum(1 for a in charge if a >= floor),
        "pack_flood": (pack[0] / lower) if pack else 0.0,
        "helmet_above_face": bool(helmet_row is not None and face_row is not None
                                  and helmet_row < face_row - 0.05 * w),
    }


def squares(prefix):
    out = {}
    for i in (1, 2, 3):
        png = os.path.join(HERE, "%s-%d.png" % (prefix, i))
        out[i] = portrait_square(png, png + ".json", "portrait-Sapper")
    return out


def engineer(prefix, i):
    png = os.path.join(HERE, "%s-%d.png" % (prefix, i))
    return portrait_square(png, png + ".json", "portrait-Engineer")


def main():
    base_only = "--base" in sys.argv[1:]
    cur = {i: measure(s) for i, s in squares("cur").items()}
    probe = {"cur": cur}
    failed = []

    sides = [cur[i]["side"] for i in (1, 2, 3)]
    if len(set(sides)) != 3:
        failed.append("crop sides are not three different rasters: %s" % sides)
    for key in ("face_share", "pack_flood"):
        vals = [cur[i][key] for i in (1, 2, 3)]
        spread = max(vals) - min(vals)
        probe["cur_%s_spread" % key] = spread
        if spread > SPREAD:
            failed.append("cur %s spread %.4f > %s" % (key, spread, SPREAD))
    # Counts are allowed to move by one between rasters: a part near the 0.004 floor, or a charge touching its
    # neighbour, splits or merges with the anti-aliasing at 119, 178 and 238 px. Measured on the cur-* captures:
    # charge_parts came out 2, 3, 3. A number this raster-sensitive is only a bar when it is asked of every single
    # capture (the fix checks below), never as "the three agree".
    if cur[2]["helmet_above_face"] != cur[1]["helmet_above_face"] or cur[3]["helmet_above_face"] != cur[1]["helmet_above_face"]:
        failed.append("cur helmet_above_face disagrees across the three captures")
    for key in ("pack_parts", "charge_parts"):
        vals = [cur[i][key] for i in (1, 2, 3)]
        if max(vals) - min(vals) > 1:
            failed.append("cur %s moves by more than one across the three captures: %s" % (key, vals))

    # What the current build is missing, measured rather than assumed: there is no face tone at all, and the
    # biggest slab of pack tone covers over half the lower disc - the owner's "tan flood". (The plan expected
    # pack_parts == 1 as well; the pixels say 2, because the satchel block already separates from the torso slab.
    # The flood, not the part count, is what catches today's portrait, so that is the bar part 2 has to beat.)
    if base_only:
        if max(cur[i]["face_share"] for i in (1, 2, 3)) >= 0.01:
            failed.append("cur face_share is already above 0.01: the baseline is not the blob the owner saw")
        if min(cur[i]["pack_flood"] for i in (1, 2, 3)) <= 0.5:
            failed.append("cur pack_flood is not over half the lower disc: the baseline is not the tan flood")

    if not base_only:
        fix = {i: measure(s) for i, s in squares("fix").items()}
        probe["fix"] = fix
        bars = {
            "face_share": max(2 * max(cur[i]["face_share"] for i in (1, 2, 3)), 0.02),
            "pack_flood": min(0.6 * min(cur[i]["pack_flood"] for i in (1, 2, 3)), 0.55),
        }
        probe["bars"] = bars
        worst_face = min(fix[i]["face_share"] for i in (1, 2, 3))
        print("face_share: cur_max=%.4f bar>=%.4f fix_min=%.4f"
              % (max(cur[i]["face_share"] for i in (1, 2, 3)), bars["face_share"], worst_face))
        if worst_face < bars["face_share"]:
            failed.append("fix face_share %.4f < bar %.4f" % (worst_face, bars["face_share"]))
        worst_flood = max(fix[i]["pack_flood"] for i in (1, 2, 3))
        print("pack_flood: cur_min=%.4f bar<=%.4f fix_max=%.4f"
              % (min(cur[i]["pack_flood"] for i in (1, 2, 3)), bars["pack_flood"], worst_flood))
        if worst_flood > bars["pack_flood"]:
            failed.append("fix pack_flood %.4f > bar %.4f" % (worst_flood, bars["pack_flood"]))
        for i in (1, 2, 3):
            m = fix[i]
            print("fix-%d (side %d): pack_parts=%d charge_parts=%d helmet_above_face=%s"
                  % (i, m["side"], m["pack_parts"], m["charge_parts"], m["helmet_above_face"]))
            if m["pack_parts"] < 2:
                failed.append("fix-%d pack_parts %d < 2 (a torso and a pack)" % (i, m["pack_parts"]))
            if m["charge_parts"] != 2:
                failed.append("fix-%d charge_parts %d != 2 (two separate charges)" % (i, m["charge_parts"]))
            if not m["helmet_above_face"]:
                failed.append("fix-%d helmet is not above the face (shoulders-up order)" % i)
        for key in ("face_share", "pack_flood"):
            vals = [fix[i][key] for i in (1, 2, 3)]
            spread = max(vals) - min(vals)
            probe["fix_%s_spread" % key] = spread
            if spread > SPREAD:
                failed.append("fix %s spread %.4f > %s" % (key, spread, SPREAD))
        drift = diff_px(engineer("cur", 2), engineer("fix", 2))
        probe["engineer_drift_px_cur_vs_fix"] = drift
        if drift != 0:
            failed.append("engineer_drift_px_cur_vs_fix %d != 0: the repaint touched another card" % drift)

    with open(os.path.join(HERE, "probe.json"), "w", encoding="utf-8") as f:
        json.dump(probe, f, indent=2)
    print(json.dumps(probe, indent=2))
    for line in failed:
        print("FAIL " + line)
    return 1 if failed else 0


if __name__ == "__main__":
    raise SystemExit(main())
