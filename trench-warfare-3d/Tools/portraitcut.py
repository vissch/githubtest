"""Cut a machine's portrait render (Tools/mechsplit.py TW_BATTLE=1 writes <renderdir>/<Name>_portrait.png) into the two
pictures the HUD reads: UI/Resources/UnitArt/<Name>.png (512 px, the cutout the armoury and the dialogue strip show) and
UI/Skin/Portraits/<Name>.png (256 px, the card's .tw-portrait-<Name>). The render is cropped to what is not transparent,
centred on a square with a margin, and given the dark ink outline the painted portraits have.

usage (from trench-warfare-3d/): python Tools/portraitcut.py <render.png> <Name>
It writes the two pngs and no .meta: Unity makes those on import (UI/Resources/UnitArt wants alphaIsTransparency on and
no mips, UnitArtTests; copy a neighbour's .meta if the editor is not there to do it).
"""
import pathlib, sys
from PIL import Image, ImageFilter

MARGIN = 0.05    # of the square's side, left clear round the machine
INK = (22, 20, 18)


def cut(src: Image.Image, size: int) -> Image.Image:
    im = src.convert("RGBA")
    box = im.getchannel("A").point(lambda a: 255 if a > 8 else 0).getbbox()
    if box is None:
        raise SystemExit("portraitcut: the render is empty")
    im = im.crop(box)
    side = int(max(im.size) / (1 - 2 * MARGIN))
    sq = Image.new("RGBA", (side, side), (0, 0, 0, 0))
    sq.paste(im, ((side - im.width) // 2, (side - im.height) // 2))
    sq = sq.resize((size, size), Image.LANCZOS)
    # the ink line: the silhouette grown by a few pixels, under the picture
    grow = max(2, size // 128)
    alpha = sq.getchannel("A").point(lambda a: 255 if a > 40 else 0).filter(ImageFilter.MaxFilter(2 * grow + 1))
    out = Image.new("RGBA", (size, size), INK + (0,))
    out.putalpha(alpha)
    out.alpha_composite(sq)
    return out


def main(argv):
    if len(argv) != 2:
        raise SystemExit(__doc__)
    src = Image.open(argv[0])
    name = argv[1]
    root = pathlib.Path("Assets/_Project/UI")
    for path, size in ((root / "Resources" / "UnitArt" / (name + ".png"), 512), (root / "Skin" / "Portraits" / (name + ".png"), 256)):
        cut(src, size).save(path)
        print("wrote", path, size)


if __name__ == "__main__":
    main(sys.argv[1:])
