# Grades the owner's Tripo environment set textures (Downloads/env sets, 2026-09-22) into the painted field's range and
# writes them as Assets/_Project/Resources/Env/<Set>/<Set>.jpg (2048): the sets are painted much brighter and more
# saturated than the night battlefield (orange gabions, lime moss, red and blue boards read as toys among the mud).
# Saturation is cut per set, greens hardest; the poppies' reds are kept; values are pulled a little toward a warm grey.
# usage: python envgrade.py <folder with the unzipped sets>   (sub-folders siege, weapon, stones, wooden, fence, plant)
import glob, os, sys
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.join(HERE, "..", "Assets", "_Project", "Resources", "Env")
# set key: (folder, base colour glob, saturation kept, value kept)
SETS = {
    "siege": ("Siege", "fantasy+siege+props+3d+model.fbm/*basecolor.jpg", .55, .92),
    "weapon": ("Weapons", "*.fbm/tripo_rgb_*.jpg", .58, .92),
    "stones": ("Stones", "*.fbm/tripo_rgb_*.jpg", .42, .78),
    "wooden": ("Wood", "wooden+props+3d+model.fbm/*basecolor.jpg", .50, .90),
    "fence": ("Fence", "*.fbm/tripo_rgb_*.jpg", .55, .92),
    "plant": ("Plants", "stylized+plant+3d+model.fbm/*basecolor.jpg", .55, .92),
}
WARM_GREY = (0.46, 0.43, 0.39)


def grade(image, keep_s, keep_v):
    h, s, v = image.convert("HSV").split()
    # hue masks (PIL hue runs 0..255 over the circle): the poppies' and rust's reds stay red; greens (grass, moss,
    # paint) are the brightest things in the sets and are cut hardest
    red = Image.eval(h, lambda x: 255 if x < 10 or x > 242 else 0)
    red = Image.composite(red, Image.new("L", image.size, 0), s.point(lambda x: 255 if x > 115 else 0))
    green = Image.eval(h, lambda x: 255 if 42 < x < 106 else 0)
    grey = Image.merge("RGB", (v, v, v))                      # the pixel's value: desaturate toward it
    out = Image.blend(grey, image, keep_s)
    out = Image.composite(Image.blend(grey, image, keep_s * .78).point(lambda x: x * .90), out, green)
    out = Image.composite(Image.blend(grey, image, .85), out, red)
    out = out.point(lambda x: x * keep_v)
    return Image.blend(out, Image.new("RGB", image.size, tuple(int(c * 255) for c in WARM_GREY)), .12)


def main(src_root):
    for key, (folder, pattern, keep_s, keep_v) in SETS.items():
        src = glob.glob(os.path.join(src_root, key, pattern))[0]
        image = Image.open(src).convert("RGB").resize((2048, 2048), Image.LANCZOS)
        grade(image, keep_s, keep_v).save(os.path.join(OUT, folder, folder + ".jpg"), quality=90)
        print(folder, "graded from", os.path.basename(src))


if __name__ == "__main__":
    main(sys.argv[1])
