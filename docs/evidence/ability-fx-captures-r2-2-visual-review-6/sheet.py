#!/usr/bin/env python3
"""Build picture sheets from captures already committed on this lane. Nothing here renders: every source jpg
already exists (`394d617b90`, `0c1d8bcf9f`, cards from `2956ad4853`); this script only tiles, labels and
re-saves them so the owner can see what the numbers describe. `--verify` rebuilds each sheet into a temp file
and byte-compares it with the committed jpg, and recomputes every source's sha1 against `sheet-manifest.json`:
the whole test is that the sheets show these captures unaltered, and nothing else.
"""
import hashlib
import json
import sys
import tempfile
from pathlib import Path

from PIL import Image, ImageDraw, ImageFont

HERE = Path(__file__).resolve().parent

DEEP = "../ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g2-g2"
PALE = "../ability-fx-captures-r2-2-g2-g1-g2-g2-g2"
CARDS = "../ability-fx-captures-r2-2-g2-g1/cards"

TILE_W, TILE_H = 640, 360
CAPTION_H = 30
TITLE_H = 36
BG = (0, 0, 0)
FG = (255, 255, 255)

# Every tile drawn onto any sheet: (sheet, row, col, source jpg relative to this folder, caption, key quoted).
TILES = [
    # sheet-t1.jpg -- the game camera, three runs, plate vs gas
    ("sheet-t1", 0, 0, f"{DEEP}/run1/gas-t1-plate.jpg", "run1  t1=557  no gas (plate)", ""),
    ("sheet-t1", 0, 1, f"{DEEP}/run1/gas-t1.jpg", "run1  t1=557  gas  bank_paint_share=0.2037", "bank_paint_share"),
    ("sheet-t1", 1, 0, f"{DEEP}/run2/gas-t1-plate.jpg", "run2  t1=597  no gas (plate)", ""),
    ("sheet-t1", 1, 1, f"{DEEP}/run2/gas-t1.jpg", "run2  t1=597  gas  bank_paint_share=0.1886", "bank_paint_share"),
    ("sheet-t1", 2, 0, f"{DEEP}/run3/gas-t1-plate.jpg", "run3  t1=637  no gas (plate)", ""),
    ("sheet-t1", 2, 1, f"{DEEP}/run3/gas-t1.jpg", "run3  t1=637  gas  bank_paint_share=0.2152", "bank_paint_share"),
    # sheet-o240.jpg -- the overview, three runs, plate vs gas
    ("sheet-o240", 0, 0, f"{DEEP}/run1/gas-o240-plate.jpg", "run1  o240=249  no gas (plate)", ""),
    ("sheet-o240", 0, 1, f"{DEEP}/run1/gas-o240.jpg",
     "run1  o240=249  gas  o240_area_frac=0.0171  bank_width_m=8.986", "o240_area_frac"),
    ("sheet-o240", 1, 0, f"{DEEP}/run2/gas-o240-plate.jpg", "run2  o240=269  no gas (plate)", ""),
    ("sheet-o240", 1, 1, f"{DEEP}/run2/gas-o240.jpg",
     "run2  o240=269  gas  o240_area_frac=0.0193  bank_width_m=8.963", "o240_area_frac"),
    ("sheet-o240", 2, 0, f"{DEEP}/run3/gas-o240-plate.jpg", "run3  o240=289  no gas (plate)", ""),
    ("sheet-o240", 2, 1, f"{DEEP}/run3/gas-o240.jpg",
     "run3  o240=289  gas  o240_area_frac=0.0195  bank_width_m=8.405", "o240_area_frac"),
    # sheet-cards.jpg -- the four isolation stills the probe tests against
    ("sheet-cards", 0, 0, f"{CARDS}/cards-plate.jpg", "plate (no FX)", ""),
    ("sheet-cards", 0, 1, f"{CARDS}/cards-gas.jpg", "gas -- accepted", ""),
    ("sheet-cards", 1, 0, f"{CARDS}/cards-smoke.jpg", "smoke -- rejected", ""),
    ("sheet-cards", 1, 1, f"{CARDS}/cards-puff.jpg", "puff -- rejected", ""),
    # sheet-before-after.jpg -- the pale round's run1 vs this round's run1, same poses (t1=557, o240=249)
    ("sheet-before-after", 0, 0, f"{PALE}/run1/gas-t1.jpg",
     "BEFORE (pale sheet)  t1=557  bank_paint_share=0.1855  bank_aspect=4.918", "bank_paint_share"),
    ("sheet-before-after", 0, 1, f"{PALE}/run1/gas-o240.jpg",
     "BEFORE (pale sheet)  o240=249  bank_patch_count=10", "bank_patch_count"),
    ("sheet-before-after", 1, 0, f"{DEEP}/run1/gas-t1.jpg",
     "AFTER (this build)  t1=557  bank_paint_share=0.2037  bank_aspect=3.452", "bank_paint_share"),
    ("sheet-before-after", 1, 1, f"{DEEP}/run1/gas-o240.jpg",
     "AFTER (this build)  o240=249  bank_patch_count=5", "bank_patch_count"),
]

SHEETS = {
    "sheet-t1": {"title": "Game camera (T1): no gas vs gas, three runs", "rows": 3, "cols": 2},
    "sheet-o240": {"title": "Overview (O240): no gas vs gas, three runs", "rows": 3, "cols": 2},
    "sheet-cards": {"title": "Isolation cards: what the probe tests against", "rows": 2, "cols": 2},
    "sheet-before-after": {"title": "Before / after: the pale sheet vs this build, same poses", "rows": 2, "cols": 2},
}


def _font():
    try:
        return ImageFont.load_default(size=18)
    except TypeError:
        return ImageFont.load_default()


FONT = _font()


def _draw_caption(draw, x, y, w, h, text):
    draw.rectangle([x, y, x + w, y + h], fill=BG)
    bbox = draw.textbbox((0, 0), text, font=FONT)
    tw, th = bbox[2] - bbox[0], bbox[3] - bbox[1]
    draw.text((x + max(4, (w - tw) // 2), y + max(2, (h - th) // 2)), text, font=FONT, fill=FG)


def build_sheet(name):
    spec = SHEETS[name]
    rows, cols = spec["rows"], spec["cols"]
    cell_w, cell_h = TILE_W, TILE_H + CAPTION_H
    img = Image.new("RGB", (cols * cell_w, TITLE_H + rows * cell_h), BG)
    draw = ImageDraw.Draw(img)
    _draw_caption(draw, 0, 0, img.width, TITLE_H, spec["title"])

    tiles = [t for t in TILES if t[0] == name]
    for _, row, col, src, caption, _key in tiles:
        path = (HERE / src).resolve()
        tile = Image.open(path).convert("RGB").resize((TILE_W, TILE_H))
        x = col * cell_w
        y = TITLE_H + row * cell_h
        img.paste(tile, (x, y))
        _draw_caption(draw, x, y + TILE_H, TILE_W, CAPTION_H, caption)
    return img


def sha1_of(path):
    return hashlib.sha1(Path(path).read_bytes()).hexdigest()


def build_all(out_dir):
    manifest = []
    for name in SHEETS:
        img = build_sheet(name)
        out_path = out_dir / f"{name}.jpg"
        img.save(out_path, "JPEG", quality=88)
    for sheet, row, col, src, caption, key in TILES:
        path = (HERE / src).resolve()
        digest = sha1_of(path)
        manifest.append({
            "sheet": sheet,
            "cell": [row, col],
            "source": src,
            "sha1": digest,
            "caption": caption,
            "key": key,
        })
        print(f"{sheet} [{row},{col}] {src} sha1={digest}")
    return manifest


def main():
    verify = "--verify" in sys.argv
    if not verify:
        manifest = build_all(HERE)
        (HERE / "sheet-manifest.json").write_text(json.dumps(manifest, indent=2) + "\n")
        return 0

    with tempfile.TemporaryDirectory() as tmp:
        tmp_dir = Path(tmp)
        manifest = build_all(tmp_dir)

        recorded = json.loads((HERE / "sheet-manifest.json").read_text())
        if len(recorded) != len(manifest):
            print(f"FAIL manifest tile count: {len(recorded)} recorded vs {len(manifest)} built")
            return 1
        for rec, built in zip(recorded, manifest):
            if rec["source"] != built["source"] or rec["sha1"] != built["sha1"]:
                print(f"FAIL source changed: {rec['source']}")
                return 1
            cur_sha1 = sha1_of((HERE / built["source"]).resolve())
            if cur_sha1 != rec["sha1"]:
                print(f"FAIL source sha1 drifted: {built['source']}")
                return 1

        for name in SHEETS:
            committed = HERE / f"{name}.jpg"
            built = tmp_dir / f"{name}.jpg"
            if committed.read_bytes() != built.read_bytes():
                print(f"FAIL sheet bytes differ: {name}.jpg")
                return 1

    print("OK: all sheets and sources match the committed captures")
    return 0


if __name__ == "__main__":
    sys.exit(main())
