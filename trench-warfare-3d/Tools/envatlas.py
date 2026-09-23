# Packs the Tripo environment sheets (six sets, and the village houses since 2026-09-23) into one atlas, each halved in size.
#
# Why: BattlefieldKit was loading Resources/Env/<Set>/<Set>.jpg for each of the six sets, six 2048x2048 sheets
# importing to about 2.8 MB of compressed VRAM apiece — roughly 17 MB to draw props that are mostly a metre across.
# Six textures is also six bindings, and SetPass was measured at 321 against 374 draw calls, very nearly one state
# change per draw.
#
# The sheets go into a 4 x 2 grid of 1024x1024 cells, which makes the sheet 4096x2048.
# It is 4 x 2 and not 3 x 2 because 3072 is not a power of two: measured 2026-09-22, Unity's default NPOT rule
# rounded a 3072-wide sheet up to 4096 and resampled it, and forcing it to keep 3072 instead cost the block
# compressor entirely — it came back as uncompressed RGB24 at 25 MB, worse than the six sheets it replaced.
# A power-of-two sheet compresses to DXT1 at about 11 MB and leaves two cells spare for a seventh set. The shader needs no change at all: TW/Toon (URP) already
# transforms its UVs by _BaseMap_ST, so a set is selected by giving its material the cell's scale and offset.
# Prop UVs live inside 0..1 of their own sheet, so scale (1/3, 1/2) and the cell's offset put them in the right cell.
#
# Unity's V axis runs from the bottom, PIL's Y from the top, so cell row 0 (drawn at the top) is the UPPER half of
# the atlas and gets V offset 0.5. SETS below is the authority for that order; BattlefieldKit.EnvCell mirrors it and
# the two must not drift apart.
#
# Run from trench-warfare-3d:  python Tools/envatlas.py
import os
from PIL import Image

SETS = ["Fence", "Plants", "Siege", "Stones", "Weapons", "Wood", "Houses", "Military"]
CELL = 1024
COLS, ROWS = 4, 2
ROOT = os.path.join("Assets", "_Project", "Resources", "Env")
OUT = os.path.join(ROOT, "EnvAtlas.jpg")   # JPEG like the sheets it replaces: Unity recompresses to DXT anyway,
                                           # and a lossless copy would put 7 MB in the repo to buy nothing on the GPU


def main():
    atlas = Image.new("RGB", (CELL * COLS, CELL * ROWS), (18, 18, 20))
    for i, name in enumerate(SETS):
        path = os.path.join(ROOT, name, name + ".jpg")
        if not os.path.exists(path):
            raise SystemExit("missing sheet: " + path)
        sheet = Image.open(path).convert("RGB")
        if sheet.size != (2048, 2048):
            raise SystemExit("%s is %s, expected 2048x2048 — the cell maths assumes it" % (name, sheet.size))
        col, row = i % COLS, i // COLS
        # LANCZOS rather than a box filter: these sheets are painted, and their edges matter more than their noise
        atlas.paste(sheet.resize((CELL, CELL), Image.LANCZOS), (col * CELL, row * CELL))
        print("%-8s -> cell (%d,%d)  uv offset (%.4f, %.4f)" % (name, col, row, col / COLS, 0.5 - row * 0.5))
    atlas.save(OUT, quality=94, subsampling=0, optimize=True)
    before = sum(os.path.getsize(os.path.join(ROOT, s, s + ".jpg")) for s in SETS)
    print("\n%s  %dx%d" % (OUT, atlas.width, atlas.height))
    print("on disk: %.1f KB for %d sheets -> %.1f KB for one" % (before / 1024, len(SETS), os.path.getsize(OUT) / 1024))
    # what actually matters is the decompressed cost, which is pixels, not the file
    print("pixels:  %.1f M -> %.1f M (%.1fx less)" % (
        len(SETS) * 2048 * 2048 / 1e6, atlas.width * atlas.height / 1e6,
        len(SETS) * 2048 * 2048 / float(atlas.width * atlas.height)))


if __name__ == "__main__":
    main()
