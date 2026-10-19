"""Cut a card portrait out of a pair of CaptureRig stills: the man, and the same pose with the ground alone.

Tools/portraitcut.py cuts a machine render that already has an alpha channel. A battlefield still has none - the man
stands in front of mud - so the alpha here is made by difference: a pixel belongs to the man where the two stills
differ by more than THRESH on any channel. Speckle (a flickering ember, a grass blade) is dropped with a min/max
filter pair, the largest remaining blob is kept, and the result is cropped, squared, resized to 256 and given the
same dark ink outline the painted portraits have (margin, LANCZOS and INK copied from Tools/portraitcut.py).

usage (from the repo root):
  python docs/evidence/special-portraits-r2/portrait_cut.py <man.png> <empty.png> <Name> [--top 0.62] [--lift 1.6]

--top keeps the upper part of the silhouette (the tanks, the wand, the plate), which is what makes the figure fill
the square: a whole standing man in a square reads near 17 % opaque and UiSkinVerifier.CheckPortrait wants 12-80 %.
--lift brightens a night figure so the card picture is not a black lump. It prints opaque_frac; aim 0.20-0.60.
It writes Assets/_Project/UI/Skin/Portraits/<Name>.png and no .meta: the committed .meta files stay as they are.
"""
import argparse
import pathlib
import sys

from PIL import Image, ImageChops, ImageFilter

MARGIN = 0.05            # of the square's side, left clear round the figure (Tools/portraitcut.py)
INK = (22, 20, 18)
THRESH = 18              # per channel, out of 255: below this the pixel is the same ground in both stills
SIZE = 256


def mask_of(man: Image.Image, empty: Image.Image) -> Image.Image:
    """White where the man shot differs from the empty shot, cleaned of speckle, largest blob only."""
    a, b = man.convert("RGB"), empty.convert("RGB")
    if a.size != b.size:
        b = b.resize(a.size, Image.LANCZOS)
    diff = ImageChops.difference(a, b)
    # the loudest channel decides: a dark figure on dark mud still moves one channel
    m = diff.convert("L")
    bands = diff.split()
    for band in bands[1:]:
        m = ImageChops.lighter(m, band)
    m = ImageChops.lighter(m, bands[0])
    m = m.point(lambda v: 255 if v > THRESH else 0)
    m = m.filter(ImageFilter.MinFilter(3))      # drop single-pixel speckle
    m = m.filter(ImageFilter.MaxFilter(5))      # close the holes the min filter opened
    return largest_blob(m)


def largest_blob(m: Image.Image) -> Image.Image:
    """Keep the biggest connected white region; a stray ember far from the man is not part of him."""
    w, h = m.size
    px = m.load()
    seen = bytearray(w * h)
    best, best_n = None, 0
    for sy in range(h):
        for sx in range(w):
            if px[sx, sy] == 0 or seen[sy * w + sx]:
                continue
            stack, blob = [(sx, sy)], []
            seen[sy * w + sx] = 1
            while stack:
                x, y = stack.pop()
                blob.append((x, y))
                for nx, ny in ((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)):
                    if 0 <= nx < w and 0 <= ny < h and not seen[ny * w + nx] and px[nx, ny]:
                        seen[ny * w + nx] = 1
                        stack.append((nx, ny))
            if len(blob) > best_n:
                best, best_n = blob, len(blob)
    out = Image.new("L", (w, h), 0)
    if best:
        o = out.load()
        for x, y in best:
            o[x, y] = 255
    return out


def cut(man_path: str, empty_path: str, top: float, lift: float):
    man = Image.open(man_path).convert("RGBA")
    empty = Image.open(empty_path)
    mask = mask_of(man, empty)
    box = mask.getbbox()
    if box is None:
        raise SystemExit("portrait_cut: the two stills are identical - no man was photographed")
    l, t, r, b = box
    if 0 < top < 1:
        b = t + max(8, int((b - t) * top))      # the upper body: tanks, wand, plate, head
    man = man.crop((l, t, r, b))
    mask = mask.crop((l, t, r, b))
    if lift and lift != 1.0:
        man = Image.merge("RGBA", [ch.point(lambda v: min(255, int(v * lift))) for ch in man.split()[:3]] + [man.split()[3]])
    man.putalpha(mask)

    side = int(max(man.size) / (1 - 2 * MARGIN))
    sq = Image.new("RGBA", (side, side), (0, 0, 0, 0))
    sq.paste(man, ((side - man.width) // 2, (side - man.height) // 2))
    sq = sq.resize((SIZE, SIZE), Image.LANCZOS)

    # the ink line: the silhouette grown by a few pixels, under the picture
    grow = max(2, SIZE // 128)
    alpha = sq.getchannel("A").point(lambda a: 255 if a > 40 else 0).filter(ImageFilter.MaxFilter(2 * grow + 1))
    out = Image.new("RGBA", (SIZE, SIZE), INK + (0,))
    out.putalpha(alpha)
    out.alpha_composite(sq)
    # the corners must be clear: CheckPortrait fails on any corner that is not transparent
    for cx, cy in ((0, 0), (SIZE - 1, 0), (0, SIZE - 1), (SIZE - 1, SIZE - 1)):
        if out.getpixel((cx, cy))[3] != 0:
            raise SystemExit("portrait_cut: a corner is opaque - the silhouette fills the frame, lower --top")
    opaque = sum(1 for p in out.getdata() if p[3] > 32) / float(SIZE * SIZE)
    return out, opaque


def main(argv):
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("man")
    ap.add_argument("empty")
    ap.add_argument("name")
    ap.add_argument("--top", type=float, default=0.62)
    ap.add_argument("--lift", type=float, default=1.0)
    ap.add_argument("--out", default="trench-warfare-3d/Assets/_Project/UI/Skin/Portraits")
    a = ap.parse_args(argv)
    img, opaque = cut(a.man, a.empty, a.top, a.lift)
    path = pathlib.Path(a.out) / (a.name + ".png")
    img.save(path)
    print("wrote %s  opaque_frac %.4f" % (path, opaque))
    if not 0.12 < opaque < 0.80:
        print("WARNING: outside UiSkinVerifier's 12-80 %% bar", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
