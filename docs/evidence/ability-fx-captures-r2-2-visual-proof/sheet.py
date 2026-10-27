"""Lay out the ability-fx-captures-r2* chain's own committed stills as three JPEGs.

No Unity, no network, no new captures: every pixel comes from a still already committed
somewhere under docs/evidence/ability-fx-captures-r2*. Run with --check to prove the
committed sheets are exactly what this script makes from those stills today.
"""
import glob
import io
import json
import os
import sys

from PIL import Image, ImageDraw, ImageFont

HERE = os.path.dirname(os.path.abspath(__file__))
EVIDENCE_DIR = os.path.dirname(HERE)

TILE_W, TILE_H, LABEL_H = 640, 360, 30
COLS = 3
JPEG_QUALITY = 85

# One short line per round, oldest first, taken from that round's commit subject
# (`git log --oneline`; the newest round's own README repeats it as its H1).
LEVERS = {
    "r2": "the chlorine bank",
    "r2-2-g2": "a gas measure three captures can agree on",
    "r2-2-g2-g1": "one gas bank, and a measure that can see a zebra",
    "r2-2-g2-g1-g2": "three captures on three frames, and a bar on the paint",
    "r2-2-g2-g1-g2-g2": "paint by direction, not hue",
    "r2-2-g2-g1-g2-g2-g2": "the gas test rejects smoke and the puff card",
    "r2-2-g2-g1-g2-g2-g2-g1": "one low chlorine bank, not a sheet of stripes",
    "r2-2-g2-g1-g2-g2-g2-g1-g1": "the gaps are closed, one bank",
    "r2-2-g2-g1-g2-g2-g2-g1-g1-g1": "a measure for the row gap, not the blob",
    "r2-2-g2-g1-g2-g2-g2-g1-g1-g1-g2": "stratified depth, wider cards",
    "r2-2-g2-g1-g2-g2-g2-g1-g1-g1-g2-g1": "thickness from the neighbourhood",
    "r2-2-g2-g1-g2-g2-g2-g1-g1-g1-g2-g1-g1": "max(own, mean) lifts the fringe",
    "r2-2-g2-g1-g2-g2-g2-g1-g1-g1-g2-g1-g1-g1": "the raised stand floor closes the islands",
    "r2-2-g2-g1-g2-g2-g2-g1-g1-g1-g2-g1-g1-g1-g1":
        "full load closes the islands; the row measure cannot reach its cap",
}


def find_rounds():
    self_name = os.path.basename(HERE)
    names = [
        os.path.basename(p)
        for p in glob.glob(os.path.join(EVIDENCE_DIR, "ability-fx-captures-r2*"))
        if os.path.isdir(p) and os.path.basename(p) != self_name
    ]
    names.sort(key=len)
    if len(names) != 14:
        sys.exit("expected 14 ability-fx-captures-r2* rounds, found %d: %s" % (len(names), names))
    for a, b in zip(names, names[1:]):
        if not b.startswith(a):
            sys.exit("chain order broken: %r is not a prefix of %r (%s)" % (a, b, names))
    return names


def round_tail(name):
    return name[len("ability-fx-captures-"):]


def round_lever(name):
    tail = round_tail(name)
    lever = LEVERS.get(tail)
    if lever is None:
        sys.exit("no LEVERS entry for round %r (tail %r)" % (name, tail))
    return lever


def round_dir(name):
    return os.path.join(EVIDENCE_DIR, name)


def pick_still(name, filename):
    run1 = os.path.join(round_dir(name), "run1", filename)
    if os.path.exists(run1):
        return run1
    flat = os.path.join(round_dir(name), filename)
    if os.path.exists(flat):
        return flat
    sys.exit("round %r has neither run1/%s nor %s" % (name, filename, filename))


def round_probe(name):
    path = os.path.join(round_dir(name), "probe.json")
    if not os.path.exists(path):
        return {}
    with open(path, "r", encoding="utf-8") as f:
        return json.load(f)


def round_metrics(name):
    probe = round_probe(name)
    run1 = probe.get("runs", {}).get("run1", {})
    patch_count = run1.get("bank_patch_count", "-")
    paint_share = run1.get("bank_paint_share", "-")
    if isinstance(paint_share, float):
        paint_share = "%.2f" % paint_share
    return patch_count, paint_share


def load_font():
    try:
        return ImageFont.truetype("arial.ttf", 18)
    except Exception:
        return ImageFont.load_default()


def make_tile(image_path, label, font):
    im = Image.open(image_path).convert("RGB").resize((TILE_W, TILE_H))
    cell = Image.new("RGB", (TILE_W, TILE_H + LABEL_H), (16, 16, 16))
    cell.paste(im, (0, 0))
    draw = ImageDraw.Draw(cell)
    draw.text((4, TILE_H + 6), label, fill=(255, 255, 255), font=font)
    return cell


def grid(tiles):
    cols = COLS
    rows = (len(tiles) + cols - 1) // cols
    cell_w, cell_h = tiles[0].size
    sheet = Image.new("RGB", (cell_w * cols, cell_h * rows), (16, 16, 16))
    for i, tile in enumerate(tiles):
        x = (i % cols) * cell_w
        y = (i // cols) * cell_h
        sheet.paste(tile, (x, y))
    return sheet


def build_chain_t1(rounds, font, sources):
    newest = rounds[-1]
    tiles = [make_tile(pick_still(newest, "gas-t1-plate.jpg"), "no gas (plate)", font)]
    sources.append(pick_still(newest, "gas-t1-plate.jpg"))
    for i, name in enumerate(rounds, start=1):
        path = pick_still(name, "gas-t1.jpg")
        sources.append(path)
        patch_count, paint_share = round_metrics(name)
        label = "%d %s: %s (patches=%s paint=%s)" % (
            i, round_tail(name), round_lever(name), patch_count, paint_share
        )
        tiles.append(make_tile(path, label, font))
    return grid(tiles)


def build_chain_o240(rounds, font, sources):
    tiles = []
    for i, name in enumerate(rounds, start=1):
        path = pick_still(name, "gas-o240.jpg")
        sources.append(path)
        patch_count, paint_share = round_metrics(name)
        label = "%d %s: %s (patches=%s paint=%s)" % (
            i, round_tail(name), round_lever(name), patch_count, paint_share
        )
        tiles.append(make_tile(path, label, font))
    return grid(tiles)


def build_before_after_t1(rounds, font, sources):
    oldest, newest = rounds[0], rounds[-1]
    plate_path = pick_still(newest, "gas-t1-plate.jpg")
    oldest_path = pick_still(oldest, "gas-t1.jpg")
    newest_path = pick_still(newest, "gas-t1.jpg")
    sources += [plate_path, oldest_path, newest_path]

    def full_tile(path, label):
        im = Image.open(path).convert("RGB")
        cell = Image.new("RGB", (im.width, im.height + 40), (16, 16, 16))
        cell.paste(im, (0, 0))
        draw = ImageDraw.Draw(cell)
        draw.text((6, im.height + 8), label, fill=(255, 255, 255), font=font)
        return cell

    tiles = [
        full_tile(plate_path, "no gas"),
        full_tile(oldest_path, "round 1 (%s)" % round_tail(oldest)),
        full_tile(newest_path, "round 14 (%s)" % round_tail(newest)),
    ]
    w = sum(t.width for t in tiles)
    h = max(t.height for t in tiles)
    sheet = Image.new("RGB", (w, h), (16, 16, 16))
    x = 0
    for t in tiles:
        sheet.paste(t, (x, 0))
        x += t.width
    return sheet


def save(sheet, path):
    sheet.save(path, "JPEG", quality=JPEG_QUALITY)


def build_all():
    rounds = find_rounds()
    font = load_font()
    chain_t1_sources = []
    chain_o240_sources = []
    before_after_sources = []
    chain_t1 = build_chain_t1(rounds, font, chain_t1_sources)
    chain_o240 = build_chain_o240(rounds, font, chain_o240_sources)
    before_after = build_before_after_t1(rounds, font, before_after_sources)
    sheets = {
        "chain-t1.jpg": chain_t1,
        "chain-o240.jpg": chain_o240,
        "before-after-t1.jpg": before_after,
    }
    return sheets, chain_t1_sources, chain_o240_sources, before_after_sources


def main():
    check = "--check" in sys.argv
    sheets, chain_t1_sources, chain_o240_sources, before_after_sources = build_all()
    if check:
        ok = True
        for name, sheet in sheets.items():
            path = os.path.join(HERE, name)
            if not os.path.exists(path):
                print("%s: MISSING" % name)
                ok = False
                continue
            buf = io.BytesIO()
            save(sheet, buf)
            with open(path, "rb") as f:
                committed = f.read()
            if buf.getvalue() == committed:
                print("%s: match" % name)
            else:
                print("%s: MISMATCH" % name)
                ok = False
        sys.exit(0 if ok else 1)

    for name, sheet in sheets.items():
        save(sheet, os.path.join(HERE, name))
    for src in chain_t1_sources:
        print(src)


if __name__ == "__main__":
    main()
