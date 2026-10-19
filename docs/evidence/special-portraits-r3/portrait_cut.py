"""Cut a card portrait out of CaptureRig stills: the man, the same pose with his prop dropped, and the bare ground.

This is special-portraits-r2's cutter with three changes, all forced by the harsh critic's r1 reading (46/100):

1. r2 kept the LARGEST connected blob only. The Sentry's plate stands on its own post, so the difference mask holds
   it as a second blob and r2 threw it away - that is the critic's "he stands in the open with a stick". Here every
   blob whose bbox touches the largest blob's bbox grown by NEAR px is kept, so the kit beside the man comes along
   and a stray ember far off still does not.
2. --top defaults to 0.85, not 0.62: the plate sits below the Sentry's chest and 0.62 cut it out of frame.
3. --prop-out cuts the third still (the man without his wand or his plate) through the SAME crop box, the same mask,
   the same square and the same resize, so the two 256x256 pictures differ in the prop's pixels and nowhere else.
   probe.py counts those pixels; that count is what proves the kit reads.

MARGIN, INK, SIZE, THRESH, the corner check and the opaque_frac print are r2's, unchanged.

usage (from the repo root):
  python docs/evidence/special-portraits-r3/portrait_cut.py <man.png> <empty.png> <Name> \
         [--noprop <noprop.png> --prop-out <out.png>] [--top 0.85] [--lift 1.6]

It prints opaque_frac; UiSkinVerifier.CheckPortrait wants 0.12-0.80. Lower --top if a corner comes back opaque.
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
NEAR = 8                 # px the largest blob's bbox grows by; a blob touching it is kit, not speckle


def mask_of(man, empty):
    """White where the man shot differs from the empty shot, cleaned of speckle, the man and his kit kept."""
    a, b = man.convert("RGB"), empty.convert("RGB")
    if a.size != b.size:
        b = b.resize(a.size, Image.LANCZOS)
    diff = ImageChops.difference(a, b)
    # the loudest channel decides: a dark figure on dark mud still moves one channel
    m = diff.convert("L")
    bands = diff.split()
    for band in bands:
        m = ImageChops.lighter(m, band)
    m = m.point(lambda v: 255 if v > THRESH else 0)
    m = m.filter(ImageFilter.MinFilter(3))      # drop single-pixel speckle
    m = m.filter(ImageFilter.MaxFilter(5))      # close the holes the min filter opened
    return figure_blobs(m)


def figure_blobs(m):
    """Keep the biggest connected region and every region whose bbox touches its bbox grown by NEAR px."""
    w, h = m.size
    px = m.load()
    seen = bytearray(w * h)
    blobs = []
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
            blobs.append(blob)
    out = Image.new("L", (w, h), 0)
    if not blobs:
        return out
    blobs.sort(key=len, reverse=True)

    def bbox(blob):
        xs = [p[0] for p in blob]
        ys = [p[1] for p in blob]
        return min(xs), min(ys), max(xs), max(ys)

    l, t, r, b = bbox(blobs[0])
    nl, nt, nr, nb = l - NEAR, t - NEAR, r + NEAR, b + NEAR
    keep = [blobs[0]]
    for blob in blobs[1:]:
        bl, bt, br, bb = bbox(blob)
        if bl <= nr and br >= nl and bt <= nb and bb >= nt:
            keep.append(blob)
    o = out.load()
    for blob in keep:
        for x, y in blob:
            o[x, y] = 255
    return out


def square(src, mask, box, lift):
    """Crop src and mask to the same box, lift, square, resize and ink. The whole geometry lives here once."""
    img = src.crop(box)
    msk = mask.crop(box)
    if lift and lift != 1.0:
        img = Image.merge("RGBA", [ch.point(lambda v: min(255, int(v * lift))) for ch in img.split()[:3]] + [img.split()[3]])
    img.putalpha(msk)

    side = int(max(img.size) / (1 - 2 * MARGIN))
    sq = Image.new("RGBA", (side, side), (0, 0, 0, 0))
    sq.paste(img, ((side - img.width) // 2, (side - img.height) // 2))
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


def cut(man_path, empty_path, top, lift, noprop_path=None):
    man = Image.open(man_path).convert("RGBA")
    empty = Image.open(empty_path)
    mask = mask_of(man, empty)
    full = mask.getbbox()
    if full is None:
        raise SystemExit("portrait_cut: the two stills are identical - no man was photographed")
    l, t, r, b = full
    if 0 < top < 1:
        b = t + max(8, int((b - t) * top))      # the upper body: tanks, wand, plate, head
    box = (l, t, r, b)
    print("figure bbox %s -> crop %s (%d x %d px of the still)" % (full, box, r - l, b - t))
    out, opaque = square(man, mask, box, lift)
    prop = None
    if noprop_path:
        # the man's own mask on the no-prop render: where the prop was, this picture shows the ground behind it
        other = Image.open(noprop_path).convert("RGBA")
        if other.size != man.size:
            other = other.resize(man.size, Image.LANCZOS)
        prop, _ = square(other, mask, box, lift)
    return out, opaque, prop


def main(argv):
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("man")
    ap.add_argument("empty")
    ap.add_argument("name")
    ap.add_argument("--noprop")
    ap.add_argument("--prop-out", dest="prop_out")
    ap.add_argument("--top", type=float, default=0.85)
    ap.add_argument("--lift", type=float, default=1.0)
    ap.add_argument("--out", default="trench-warfare-3d/Assets/_Project/UI/Skin/Portraits")
    a = ap.parse_args(argv)
    img, opaque, prop = cut(a.man, a.empty, a.top, a.lift, a.noprop)
    path = pathlib.Path(a.out) / (a.name + ".png")
    img.save(path)
    print("wrote %s  opaque_frac %.4f" % (path, opaque))
    if prop is not None and a.prop_out:
        prop.save(a.prop_out)
        print("wrote %s (the same cut without the prop)" % a.prop_out)
    if not 0.12 < opaque < 0.80:
        print("WARNING: outside UiSkinVerifier's 12-80 % bar", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
