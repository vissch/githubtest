#!/usr/bin/env python3
"""sheet.py - tile the eight committed capture rounds of the ability-fx gas chain into labelled contact sheets.

The owner asked to be SHOWN the chain, not told about it. This shoots nothing: every still and every number comes
from the `probe.json` and the jpgs each round already committed. Captions are READ from those files, never typed
here, so a sheet cannot claim a number the round did not measure.

    python sheet.py            # write sheets/*.png and sheet-manifest.json
    python sheet.py --verify   # re-read the manifest: every source, sha1, PNG and tile count

Exit 0 when it wrote (or verified) everything, 1 on a named failing tile or a bad call.
"""
import hashlib
import json
import os
import sys

try:
    from PIL import Image, ImageChops, ImageDraw
except ImportError:
    sys.stderr.write("PIL (pillow) is needed: python -m pip install pillow\n")
    sys.exit(1)

HERE = os.path.dirname(os.path.abspath(__file__))
EVIDENCE = os.path.dirname(HERE)
SHEETS = os.path.join(HERE, "sheets")
MANIFEST = os.path.join(HERE, "sheet-manifest.json")

# The eight rounds of the chain, oldest first. Round 1 keeps its stills at the folder root; the rest have run1/.
ROUNDS = (
    "ability-fx-captures-r2",
    "ability-fx-captures-r2-2-g2",
    "ability-fx-captures-r2-2-g2-g2",
    "ability-fx-captures-r2-2-g2-g2-g2",
    "ability-fx-captures-r2-2-g2-g2-g2-g1",
    "ability-fx-captures-r2-2-g2-g2-g2-g1-g1",
    "ability-fx-captures-r2-2-g2-g2-g2-g1-g1-g2",
    "ability-fx-captures-r2-2-g2-g2-g2-g1-g1-g2-g2",
)

TILE_W, TILE_H = 420, 236     # a 1280x720 still at a third, so four fit across a sheet a person can open
CAPTION_H = 62                # four lines of PIL's default bitmap font, with room to breathe
PAD = 8
DIFF_BOOST = 4                # night stills read almost black raw; x4 is what makes the gas visible to a person

# probe.json moved its key names twice down the chain. Newest name first, older ones as fallbacks.
NUMBER_KEYS = {
    "green_height_frac": ("green_height_frac", "bank_height_frac", "gas_t1_height_frac"),
    "green_low_share": ("green_low_share", "bank_low_share", "gas_t1_green_low_share"),
    "green_rise_m": ("green_rise_m", "bank_rise_m"),
}


def sha1(path):
    h = hashlib.sha1()
    with open(path, "rb") as fh:
        for chunk in iter(lambda: fh.read(1 << 16), b""):
            h.update(chunk)
    return h.hexdigest()


def round_probe(name):
    """The flat dict of numbers for a round's run1, and its T1 frame (None when the round recorded no frames)."""
    path = os.path.join(EVIDENCE, name, "probe.json")
    if not os.path.exists(path):
        return {}, None
    with open(path, "r", encoding="utf-8") as fh:
        doc = json.load(fh)
    flat = {}
    flat.update(doc.get("rows", {}))          # round 1 is flat, with a "rows" bag beside the headline numbers
    flat.update({k: v for k, v in doc.items() if not isinstance(v, dict)})
    flat.update(doc.get("runs", {}).get("run1", {}))
    frame = doc.get("frames", {}).get("run1", {}).get("gas-t1")
    return flat, frame


def numbers_for(name):
    flat, frame = round_probe(name)
    out = {"t1_frame": frame}
    for label, keys in NUMBER_KEYS.items():
        value = None
        for key in keys:
            if key in flat and not isinstance(flat[key], dict):
                value = flat[key]
                break
        out[label] = value
    return out


def fmt(value):
    if value is None:
        return "-"
    if isinstance(value, float):
        return "%.4g" % value
    return str(value)


def stills_dir(name):
    run1 = os.path.join(EVIDENCE, name, "run1")
    return run1 if os.path.isdir(run1) else os.path.join(EVIDENCE, name)


def caption_for(name, nums):
    return [
        name,
        "T1 frame %s" % fmt(nums["t1_frame"]),
        "height_frac %s  low_share %s" % (fmt(nums["green_height_frac"]), fmt(nums["green_low_share"])),
        "rise_m %s" % fmt(nums["green_rise_m"]),
    ]


def draw_tile(sheet, img, lines, col, row):
    x = PAD + col * (TILE_W + PAD)
    y = PAD + row * (TILE_H + CAPTION_H + PAD)
    sheet.paste(img.resize((TILE_W, TILE_H), Image.LANCZOS).convert("RGB"), (x, y))
    pen = ImageDraw.Draw(sheet)
    for i, line in enumerate(lines[:4]):
        pen.text((x + 2, y + TILE_H + 3 + i * 14), line, fill=(235, 235, 235))


def blank(cols, rows):
    w = PAD + cols * (TILE_W + PAD)
    h = PAD + rows * (TILE_H + CAPTION_H + PAD)
    return Image.new("RGB", (w, h), (18, 18, 18))


def boosted_diff(still, plate):
    diff = ImageChops.difference(still.convert("RGB"), plate.convert("RGB"))
    return diff.point(lambda v: min(255, v * DIFF_BOOST))


def chain_sheet(sheet_name, still):
    """One tile per round at one camera, four across and two down."""
    sheet = blank(4, 2)
    tiles = []
    for i, name in enumerate(ROUNDS):
        src = os.path.join(stills_dir(name), still)
        if not os.path.exists(src):
            print("skipped (no such still): %s" % os.path.relpath(src, EVIDENCE))
            continue
        nums = numbers_for(name)
        lines = caption_for(name, nums)
        draw_tile(sheet, Image.open(src), lines, i % 4, i // 4)
        tiles.append({
            "sheet": sheet_name,
            "cell": "r%dc%d" % (i // 4, i % 4),
            "source": os.path.relpath(src, EVIDENCE).replace("\\", "/"),
            "sha1": sha1(src),
            "caption": " | ".join(lines),
            "numbers": nums,
        })
    sheet.save(os.path.join(SHEETS, sheet_name))
    print("wrote sheets/%s, %d tiles" % (sheet_name, len(tiles)))
    return tiles


def before_after():
    """2 rows x 3: the first round and the last, each as still, plate, and the plate difference boosted x4."""
    sheet_name = "before-after.png"
    sheet = blank(3, 2)
    tiles = []
    for row, name in enumerate((ROUNDS[0], ROUNDS[-1])):
        base = stills_dir(name)
        still_path = os.path.join(base, "gas-t1.jpg")
        plate_path = os.path.join(base, "gas-t1-plate.jpg")
        missing = [p for p in (still_path, plate_path) if not os.path.exists(p)]
        if missing:
            for p in missing:
                print("skipped (no such still): %s" % os.path.relpath(p, EVIDENCE))
            continue
        nums = numbers_for(name)
        still = Image.open(still_path)
        plate = Image.open(plate_path)
        panels = (
            (still, still_path, "%s  gas-t1" % name),
            (plate, plate_path, "%s  gas-t1-plate (no gas)" % name),
            (boosted_diff(still, plate), still_path, "%s  plate difference x%d" % (name, DIFF_BOOST)),
        )
        for col, (img, src, head) in enumerate(panels):
            lines = [head,
                     "T1 frame %s" % fmt(nums["t1_frame"]),
                     "height_frac %s  low_share %s" % (fmt(nums["green_height_frac"]),
                                                       fmt(nums["green_low_share"])),
                     "rise_m %s" % fmt(nums["green_rise_m"])]
            draw_tile(sheet, img, lines, col, row)
            tiles.append({
                "sheet": sheet_name,
                "cell": "r%dc%d" % (row, col),
                "source": os.path.relpath(src, EVIDENCE).replace("\\", "/"),
                "sha1": sha1(src),
                "caption": " | ".join(lines),
                "numbers": nums,
            })
    sheet.save(os.path.join(SHEETS, sheet_name))
    print("wrote sheets/%s, %d tiles" % (sheet_name, len(tiles)))
    return tiles


def write():
    if not os.path.isdir(SHEETS):
        os.makedirs(SHEETS)
    tiles = []
    tiles += chain_sheet("chain-t1.png", "gas-t1.jpg")
    tiles += chain_sheet("chain-o240.png", "gas-o240.jpg")
    tiles += before_after()
    counts = {}
    for tile in tiles:
        counts[tile["sheet"]] = counts.get(tile["sheet"], 0) + 1
    with open(MANIFEST, "w", encoding="utf-8") as fh:
        json.dump({"rounds": list(ROUNDS), "diff_boost": DIFF_BOOST, "tile_counts": counts, "tiles": tiles},
                  fh, indent=1, sort_keys=True)
        fh.write("\n")
    print("wrote sheet-manifest.json, %d tiles over %d sheets" % (len(tiles), len(counts)))
    return 0


def verify():
    if not os.path.exists(MANIFEST):
        sys.stderr.write("no sheet-manifest.json: run `python sheet.py` first\n")
        return 1
    with open(MANIFEST, "r", encoding="utf-8") as fh:
        doc = json.load(fh)
    bad = 0
    seen = {}
    for tile in doc["tiles"]:
        where = "%s %s" % (tile["sheet"], tile["cell"])
        src = os.path.join(EVIDENCE, tile["source"])
        seen[tile["sheet"]] = seen.get(tile["sheet"], 0) + 1
        if not os.path.exists(src):
            print("FAIL %s: source gone: %s" % (where, tile["source"]))
            bad += 1
            continue
        got = sha1(src)
        if got != tile["sha1"]:
            print("FAIL %s: %s is sha1 %s, the manifest says %s" % (where, tile["source"], got, tile["sha1"]))
            bad += 1
            continue
        print("ok   %s  %s" % (where, tile["source"]))
    for sheet_name, want in sorted(doc["tile_counts"].items()):
        path = os.path.join(SHEETS, sheet_name)
        if not os.path.exists(path):
            print("FAIL %s: the PNG is missing" % sheet_name)
            bad += 1
            continue
        try:
            Image.open(path).load()
        except Exception as err:                                  # a truncated or non-image PNG
            print("FAIL %s: will not open: %s" % (sheet_name, err))
            bad += 1
            continue
        if seen.get(sheet_name, 0) != want:
            print("FAIL %s: %d tiles in the manifest, tile_counts says %d"
                  % (sheet_name, seen.get(sheet_name, 0), want))
            bad += 1
            continue
        print("ok   %s  opens, %d tiles" % (sheet_name, want))
    if bad:
        sys.stderr.write("%d failing tile(s)\n" % bad)
        return 1
    print("verified %d tiles over %d sheets" % (len(doc["tiles"]), len(doc["tile_counts"])))
    return 0


def main(argv):
    if len(argv) > 1 and argv[1] == "--verify":
        return verify()
    if len(argv) > 1:
        sys.stderr.write(__doc__)
        return 1
    return write()


if __name__ == "__main__":
    sys.exit(main(sys.argv))
