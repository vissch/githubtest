"""Measures the Sapper card the same way sapper-look-3-g2's probe does (same crop: the sidecar's
"portrait-<Name>" rect, then the centred square of side min(w, h) since the picture is drawn
`background-size: contain` from a square 256x256 source), but kit_share and figure_cover are reported
numbers only here, never barred - the owner's point is that they pass on a tan flood (kit_share: any
pixel near pack tone counts) and on a bright lower half (figure_cover: no shape, just luma), so a flood
clears both without drawing a person.

Three new scalars are gated instead, all read from pixels inside a disc of radius 0.40*side centred on
the square (so the card's ring/plate never counts):

  pack_blob       - largest connected component of pack-tone pixels shaped like a satchel (area share
                    0.015-0.12, taller than wide, mostly filled, off the midline). 0.0 if none passes.
  charge_on_pack  - charge-tone components small enough to be charges (area share 0.003-0.03, mostly
                    filled) whose centre sits inside the passing pack_blob's bbox; needs that pack blob
                    AND at least two such charges, else 0.0. Value is their summed area share.
  person_taper    - median figure width (luma clearing the disc's own luma by >=24) per row, as a
                    fraction of the square's side, over the head band (rows 20-40% of the square) versus
                    the shoulder band (55-85%). shoulder/head when head is in [0.15, 0.45] and shoulder is
                    in [0.45, 0.95], else 0.0 - a "?" (narrow stem under a wide ring) and a flood (wide
                    everywhere) both score 0.

Part 1 of the plan: only the three base captures (card-base-{1,2,3}.png, the current build, nothing
painted) are checked here. Each of the three new scalars must be 0.0 in every base capture - the
measured proof that today's build has no pack-shaped region, no charges sitting on a pack, and a "?"
Engineer/Sapper, not a typed-in number. kit_share and figure_cover must still agree within 0.005 across
the three captures, and portrait-Assault (the control card from here on; the Engineer is repainted in a
later part) must show zero drift between captures.

Part 2 adds card-sap-{1,2,3}.png (the repainted Sapper, PaintSapperPortrait's torso now a plate tone with
the satchel the only pack-tone block): pack_blob and charge_on_pack must be > 0 in every one (the base max
is 0.0, so any positive floor sits outside the base spread), pack_blob must also stay <= 0.12 so a later
flood cannot pass the same bar, and portrait-Assault must show zero drift between card-base-1 and
card-sap-1 (the repaint touches only the Sapper's own portrait).

Later parts add card-eng-*.png (the repainted Engineer) and raise its own bars; this script already knows
how to score any capture, so later parts only add to main().
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
SHAPE_THRESHOLD = 24
DISC_RADIUS_FRACTION = 0.40

PACK_BLOB_AREA = (0.015, 0.12)
PACK_BLOB_ASPECT = (1.3, 3.5)
PACK_BLOB_FILL_MIN = 0.6
PACK_BLOB_OFFMID_MIN = 0.10

CHARGE_AREA = (0.003, 0.03)
CHARGE_FILL_MIN = 0.6

HEAD_BAND = (0.20, 0.40)
SHOULDER_BAND = (0.55, 0.85)
HEAD_RANGE = (0.15, 0.45)
SHOULDER_RANGE = (0.45, 0.95)


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


def disc_mask(w, h):
    side = min(w, h)
    cx, cy = (w - 1) / 2.0, (h - 1) / 2.0
    r = DISC_RADIUS_FRACTION * side
    mask = [[False] * w for _ in range(h)]
    for y in range(h):
        for x in range(w):
            if (x - cx) ** 2 + (y - cy) ** 2 <= r * r:
                mask[y][x] = True
    return mask


def _connected_components(hit):
    h = len(hit)
    w = len(hit[0]) if h else 0
    seen = [[False] * w for _ in range(h)]
    components = []
    for sy in range(h):
        for sx in range(w):
            if not hit[sy][sx] or seen[sy][sx]:
                continue
            stack = [(sx, sy)]
            seen[sy][sx] = True
            cells = []
            while stack:
                x, y = stack.pop()
                cells.append((x, y))
                for nx, ny in ((x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)):
                    if 0 <= nx < w and 0 <= ny < h and hit[ny][nx] and not seen[ny][nx]:
                        seen[ny][nx] = True
                        stack.append((nx, ny))
            components.append(cells)
    return components


def _tone_hits(square, mask, tone):
    px = square.load()
    w, h = square.size
    hit = [[False] * w for _ in range(h)]
    for y in range(h):
        for x in range(w):
            if not mask[y][x]:
                continue
            r, g, b = px[x, y]
            d = max(abs(r - tone[0]), abs(g - tone[1]), abs(b - tone[2]))
            if d <= SHAPE_THRESHOLD:
                hit[y][x] = True
    return hit


def _bbox(cells):
    xs = [c[0] for c in cells]
    ys = [c[1] for c in cells]
    return min(xs), min(ys), max(xs), max(ys)


def pack_blob(square):
    w, h = square.size
    side = float(min(w, h))
    area = float(w * h)
    mask = disc_mask(w, h)
    hit = _tone_hits(square, mask, PACK)
    best_share = 0.0
    best_bbox = None
    for cells in _connected_components(hit):
        share = len(cells) / area
        if not (PACK_BLOB_AREA[0] <= share <= PACK_BLOB_AREA[1]):
            continue
        x0, y0, x1, y1 = _bbox(cells)
        bw, bh = x1 - x0 + 1, y1 - y0 + 1
        if bw <= 0:
            continue
        aspect = bh / float(bw)
        if not (PACK_BLOB_ASPECT[0] <= aspect <= PACK_BLOB_ASPECT[1]):
            continue
        fill = len(cells) / float(bw * bh)
        if fill < PACK_BLOB_FILL_MIN:
            continue
        cx = (x0 + x1) / 2.0
        if abs(cx / side - 0.5) < PACK_BLOB_OFFMID_MIN:
            continue
        if share > best_share:
            best_share = share
            best_bbox = (x0, y0, x1, y1)
    return best_share, best_bbox


def charge_on_pack(square, pack_bbox):
    if pack_bbox is None:
        return 0.0
    w, h = square.size
    area = float(w * h)
    px0, py0, px1, py1 = pack_bbox
    mask = disc_mask(w, h)
    hit = _tone_hits(square, mask, CHARGE)
    total_share = 0.0
    count = 0
    for cells in _connected_components(hit):
        share = len(cells) / area
        if not (CHARGE_AREA[0] <= share <= CHARGE_AREA[1]):
            continue
        x0, y0, x1, y1 = _bbox(cells)
        bw, bh = x1 - x0 + 1, y1 - y0 + 1
        fill = len(cells) / float(bw * bh)
        if fill < CHARGE_FILL_MIN:
            continue
        cx, cy = (x0 + x1) / 2.0, (y0 + y1) / 2.0
        if not (px0 <= cx <= px1 and py0 <= cy <= py1):
            continue
        total_share += share
        count += 1
    if count < 2:
        return 0.0
    return total_share


def _median(values):
    if not values:
        return 0.0
    s = sorted(values)
    n = len(s)
    mid = n // 2
    if n % 2:
        return s[mid]
    return (s[mid - 1] + s[mid]) / 2.0


def _band_width(hit, side, w, band):
    y0 = int(round(band[0] * side))
    y1 = int(round(band[1] * side))
    widths = []
    for y in range(max(0, y0), min(len(hit), y1)):
        widths.append(sum(1 for x in range(w) if hit[y][x]) / float(side))
    return _median(widths)


def person_taper(square):
    w, h = square.size
    side = float(min(w, h))
    mask = disc_mask(w, h)
    px = square.load()
    disc_luma = luma(*DISC)
    hit = [[False] * w for _ in range(h)]
    for y in range(h):
        for x in range(w):
            if not mask[y][x]:
                continue
            r, g, b = px[x, y]
            if luma(r, g, b) - disc_luma >= LUMA_THRESHOLD:
                hit[y][x] = True
    head = _band_width(hit, side, w, HEAD_BAND)
    shoulder = _band_width(hit, side, w, SHOULDER_BAND)
    if not (HEAD_RANGE[0] <= head <= HEAD_RANGE[1]):
        return 0.0
    if not (SHOULDER_RANGE[0] <= shoulder <= SHOULDER_RANGE[1]):
        return 0.0
    return shoulder / head


def score(square):
    blob_share, blob_bbox = pack_blob(square)
    return {
        "kit_share": kit_share(square),
        "figure_cover": figure_cover(square),
        "pack_blob": blob_share,
        "charge_on_pack": charge_on_pack(square, blob_bbox),
        "person_taper": person_taper(square),
    }


def _load(stem):
    squares_sap = {}
    squares_assault = {}
    for i in (1, 2, 3):
        png = os.path.join(HERE, f"{stem}-{i}.png")
        js = png + ".json"
        squares_sap[i] = portrait_square(png, js, "portrait-Sapper")
        squares_assault[i] = portrait_square(png, js, "portrait-Assault")
    return squares_sap, squares_assault


def main():
    base_sap, base_assault = _load("card-base")
    sap_sap, sap_assault = _load("card-sap")

    base_scores = {i: score(base_sap[i]) for i in (1, 2, 3)}
    sap_scores = {i: score(sap_sap[i]) for i in (1, 2, 3)}

    kit_vals = [base_scores[i]["kit_share"] for i in (1, 2, 3)]
    cover_vals = [base_scores[i]["figure_cover"] for i in (1, 2, 3)]
    kit_spread = max(kit_vals) - min(kit_vals)
    cover_spread = max(cover_vals) - min(cover_vals)

    pairs = [(1, 2), (1, 3), (2, 3)]
    assault_drift = {f"{a}-{b}": diff_px(base_assault[a], base_assault[b]) for a, b in pairs}
    assault_drift_base_sap = diff_px(base_assault[1], sap_assault[1])

    probe = {
        "card_base": base_scores,
        "card_sap": sap_scores,
        "kit_share_base_spread": kit_spread,
        "figure_cover_base_spread": cover_spread,
        "assault_drift_px": assault_drift,
        "assault_drift_px_base1_sap1": assault_drift_base_sap,
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
    for pair, drift in assault_drift.items():
        if drift != 0:
            print(f"FAIL assault_drift_px[{pair}]: {drift} != 0")
            failed = True
    if assault_drift_base_sap != 0:
        print(f"FAIL assault_drift_px_base1_sap1: {assault_drift_base_sap} != 0")
        failed = True
    for i in (1, 2, 3):
        for key in ("pack_blob", "charge_on_pack", "person_taper"):
            val = base_scores[i][key]
            if val != 0.0:
                print(f"FAIL card_base[{i}][{key}]: {val} != 0.0")
                failed = True
    for i in (1, 2, 3):
        pb = sap_scores[i]["pack_blob"]
        cop = sap_scores[i]["charge_on_pack"]
        if not (0.0 < pb <= 0.12):
            print(f"FAIL card_sap[{i}][pack_blob]: {pb} not in (0.0, 0.12]")
            failed = True
        if not (cop > 0.0):
            print(f"FAIL card_sap[{i}][charge_on_pack]: {cop} <= 0.0")
            failed = True

    return 1 if failed else 0


if __name__ == "__main__":
    raise SystemExit(main())
