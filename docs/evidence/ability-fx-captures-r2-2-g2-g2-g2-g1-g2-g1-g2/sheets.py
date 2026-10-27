"""Contact sheets of the gas ability captures already in this folder, read-only.

Three sheets, each tile 320 px wide with a one-line label (set/run, what it is,
and the short sha the capture's bytes come from), written to look/:
  1-sheet-cards.png  the 15 card captures (5 books x 3 sets), 5 cols x 3 rows
  2-sheet-t1.png      the 6 T1 field captures (gas/plate x 3 runs), 3 cols x 2 rows
  3-sheet-o240.png    the 6 O240 field captures (gas/plate x 3 runs), 3 cols x 2 rows

--check re-renders and asserts the tile count (27 across the three sheets).
"""
import argparse
import subprocess
from pathlib import Path

from PIL import Image, ImageDraw

HERE = Path(__file__).resolve().parent
LOOK = HERE / "look"
TILE_W = 320
LABEL_H = 34
WIDE = 1600

CARD_BOOKS = ["gas", "gaspuff", "plate", "puff", "smoke"]
CARD_SETS = ["cards1", "cards2", "cards3"]

FIELD_RUNS = ["run1", "run2", "run3"]


def short_sha(rel_path):
    out = subprocess.run(
        ["git", "log", "-1", "--format=%h", "--", rel_path],
        cwd=HERE.parent.parent.parent,
        capture_output=True,
        text=True,
    )
    sha = out.stdout.strip()
    return sha if sha else "unshot"


def rel_from_repo(path):
    return str(path.relative_to(HERE.parent.parent.parent)).replace("\\", "/")


def load_tile(path, label):
    img = Image.open(path).convert("RGB")
    w, h = img.size
    tile_h = round(h * TILE_W / w)
    img = img.resize((TILE_W, tile_h))
    canvas = Image.new("RGB", (TILE_W, tile_h + LABEL_H), (20, 20, 20))
    canvas.paste(img, (0, 0))
    draw = ImageDraw.Draw(canvas)
    draw.rectangle([0, tile_h, TILE_W, tile_h + LABEL_H], fill=(20, 20, 20))
    draw.text((4, tile_h + 4), label[:60], fill=(255, 255, 255))
    draw.text((4, tile_h + 19), label[60:120], fill=(200, 200, 200))
    return canvas


def grid(tiles, cols):
    rows = -(-len(tiles) // cols)
    tw, th = tiles[0].size
    sheet = Image.new("RGB", (tw * cols, th * rows), (0, 0, 0))
    for i, t in enumerate(tiles):
        x = (i % cols) * tw
        y = (i // cols) * th
        sheet.paste(t, (x, y))
    return sheet


def cards_sheet():
    tiles = []
    for cset in CARD_SETS:
        for book in CARD_BOOKS:
            p = HERE / cset / f"cards-{book}.jpg"
            rel = rel_from_repo(p)
            label = f"{cset} cards-{book} {short_sha(rel)}"
            tiles.append(load_tile(p, label))
    return grid(tiles, 5)


def field_sheet(band):
    tiles = []
    for kind in ["", "-plate"]:
        for run in FIELD_RUNS:
            p = HERE / run / f"gas-{band}{kind}.jpg"
            rel = rel_from_repo(p)
            what = f"gas-{band}{kind}" if kind else f"gas-{band} (gas)"
            label = f"{run} {what} {short_sha(rel)}"
            tiles.append(load_tile(p, label))
    return grid(tiles, 3)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--check", action="store_true")
    args = ap.parse_args()

    LOOK.mkdir(exist_ok=True)
    sheets = {
        "1-sheet-cards.png": cards_sheet(),
        "2-sheet-t1.png": field_sheet("t1"),
        "3-sheet-o240.png": field_sheet("o240"),
    }

    tile_count = len(CARD_SETS) * len(CARD_BOOKS) + 2 * (2 * len(FIELD_RUNS))

    for name, img in sheets.items():
        assert img.width <= WIDE, f"{name} is {img.width} px wide, over {WIDE}"
        path = LOOK / name
        img.save(path)
        print(path)

    print(f"{tile_count} tiles")

    if args.check:
        assert tile_count == 27, tile_count


if __name__ == "__main__":
    main()
