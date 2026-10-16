#!/usr/bin/env python3
"""Repaint a machine's atlas as flat toon paint: the swirl out, the panel lines kept, the house trim and emblem drawn.

The Pavise came off the Tripo sheet in a blue-on-white liquid marble that follows the UVs and not the light, so its
shield (the one part a player names it by) is the loudest noise in the frame. The Redoubt is the look the game has:
flat base colour, crisp panel lines, a spiral emblem. The owner's sheet is gone, so the repaint works on the atlas.

    # 1. the UV islands, one label per mesh part (Blender 5.0, read-only on the FBX: imports, never exports)
    "C:\\Program Files\\Blender Foundation\\Blender 5.0\\blender.exe" -b --factory-startup -P Tools/crabpaint.py -- \\
        masks Assets/_Project/Resources/Vehicles Pavise %TEMP%/pavise_masks.npz
    # 2. the paint, in place on the atlas
    python Tools/crabpaint.py paint Assets/_Project/Resources/Vehicles/PaviseAtlas.jpg %TEMP%/pavise_masks.npz
    # 3. how noisy an atlas is: high is marble, low is flat paint. Writes nothing
    python Tools/crabpaint.py score Assets/_Project/Resources/Vehicles/{Pavise,Redoubt}Atlas.jpg

`paint` works island by island: it masks the marble (blue leading the other channels where the island around the
pixel is light), closes the white ground over it from a large-radius average of that island's own non-marble pixels,
median-filters and quantises luminance to a few bands around the island's own hue, then composites back a mask of
strong dark gradients taken *before* the flatten, so the panel lines and the gun port stay crisp. Last it draws
(never samples) a 2 px house-blue trim inset from each island's border and one double spiral on the Shield island.

A re-split of the machine (`crabsplit.py`) rewrites the atlas from the sheet, so the repaint is gone: run it again.
"""
import argparse
import os
import sys

# The house colours, the median of RedoubtAtlas.jpg's own emblem pixels (blue: b - max(r,g) > 0.10; gold:
# r - b > 0.12 and g - b > 0.05), measured 2026-10-16 so the Pavise wears what the Redoubt wears.
HOUSE_BLUE = (64, 56, 121)
TRIM_GOLD = (162, 96, 60)      # the Redoubt's warm trim: pass it to draw_trim(..., colour=TRIM_GOLD) for a gold edge

WIN = 24            # the chroma window of `score`, in pixels
LIGHT = 0.30        # a pixel at or over this luminance is "light"


# ------------------------------------------------------------------------------------------------- score
def chroma_noise(x, win=WIN, light=LIGHT):
    """The mean, over light pixels, of the local standard deviation of chroma in a `win` px window."""
    import numpy as np
    from scipy import ndimage
    lum = x.mean(2)
    c = x - lum[..., None]
    var = np.zeros(lum.shape, np.float64)
    for k in range(3):
        ch = c[..., k].astype(np.float64)
        m1 = ndimage.uniform_filter(ch, win)
        var += np.maximum(ndimage.uniform_filter(ch * ch, win) - m1 * m1, 0.0)
    sd = np.sqrt(var)
    sel = lum >= light
    if sel.sum() < 0.02 * lum.size:
        sel = np.ones(lum.shape, bool)
    return float(sd[sel].mean())


# ------------------------------------------------------------------------------------------------- paint
def _hue(x, m):
    """The island's own colour direction: the median chromaticity of its pixels, normalised to mean 1."""
    import numpy as np
    px = x[m].reshape(-1, 3)
    lum = np.maximum(px.mean(1, keepdims=True), 1e-3)
    h = np.median(px / lum, 0)
    return h / max(h.mean(), 1e-3)


def line_mask(x, m, thresh=0.18, chroma=0.10):
    """Strong dark gradients inside the island: panel lines, part borders, the gun port.

    Dark *and* near-grey: the marble's blue is dark too and its edges are as steep as a panel line, so a line is
    told from the swirl by having almost no colour of its own.
    """
    import numpy as np
    from scipy import ndimage
    lum = x.mean(2)
    g = np.hypot(ndimage.sobel(lum, 0), ndimage.sobel(lum, 1))
    g = ndimage.maximum_filter(g, 3)        # a 1 px line answers nothing at its own centre, only beside it
    dark = lum <= 0.6 * float(np.median(lum[m]))
    grey = (x.max(2) - x.min(2)) <= chroma
    return m & (g >= thresh) & dark & grey


def marble_mask(x, m, blue=0.07, win=WIN, light=0.45):
    """Blue leading the other two channels, where the island *around* the pixel is light (the marble ground)."""
    import numpy as np
    from scipy import ndimage
    lum = x.mean(2)
    lead = x[..., 2] - np.maximum(x[..., 0], x[..., 1])
    inside = m.astype(np.float64)
    near = ndimage.uniform_filter(lum * inside, win) / np.maximum(ndimage.uniform_filter(inside, win), 1e-6)
    return m & (lead > blue) & (near > light)


def _fill_from(x, good, radius=32):
    """A large-radius average of the `good` pixels, for closing a hole from the paint around it."""
    import numpy as np
    from scipy import ndimage
    w = good.astype(np.float64)
    den = ndimage.uniform_filter(w, 2 * radius + 1)
    num = np.stack([ndimage.uniform_filter(x[..., k] * w, 2 * radius + 1) for k in range(3)], 2)
    return num / np.maximum(den, 1e-6)[..., None], den > 1e-4


def paint_island(x, m, steps=4, blue=0.07, line=0.18, flat_radius=12):
    """One island, repainted. Returns the new colours for `m` only; `x` is not touched."""
    import numpy as np
    from scipy import ndimage
    med = np.median(x[m].reshape(-1, 3), 0)
    lines = line_mask(x, m, line)
    marble = marble_mask(x, m, blue) & ~lines
    # a. the swirl out: the white ground closes over the blue
    y = x.copy()
    fill, have = _fill_from(x, m & ~marble)
    hole = marble & have
    y[hole] = fill[hole]
    y[marble & ~have] = med
    # b. flat: a median filter, then luminance in a few bands around the island's own hue
    size = 2 * flat_radius + 1
    sm = np.stack([ndimage.median_filter(y[..., k], size=size, mode='nearest') for k in range(3)], 2)
    l = sm.mean(2)
    # a fixed 1/steps grid of luminance, not the island's own percentiles: a band centre quantises to itself, so
    # painting an already-painted atlas is a no-op instead of drifting a little further every run
    band = np.clip(np.floor(l * steps), 0, steps - 1)
    level = (band + 0.5) / steps
    # the base hue comes from the island's *kept* pixels, never from the swirl, and a level that would clip a
    # channel is desaturated toward grey instead of clipped, so luminance stays on its band and a second run agrees
    hue = _hue(x, (m & ~marble) if (m & ~marble).sum() > 64 else m)
    c = level[..., None] * hue[None, None, :]
    mx = c.max(2)
    t = np.where(mx > 1.0, (mx - 1.0) / np.maximum(mx - level, 1e-6), 0.0)[..., None]
    out = np.clip(c * (1.0 - t) + level[..., None] * t, 0.0, 1.0)
    # c. the lines back, as they were
    out[lines] = x[lines]
    return out[m]


def draw_trim(img, m, colour=HOUSE_BLUE, width=2, inset=2):
    """A `width` px outline inset from the island's border, drawn into the uint8 image in place."""
    from scipy import ndimage
    inner = ndimage.binary_erosion(m, iterations=inset + width)
    ring = ndimage.binary_erosion(m, iterations=inset) & ~inner
    img[ring] = colour
    return ring


def draw_spiral(img, m, colour=HOUSE_BLUE, turns=2.0, width=3):
    """One crisp double spiral at the island's centre, about a third of its shorter side, clipped to the island."""
    import numpy as np
    from PIL import Image, ImageDraw
    ys, xs = np.nonzero(m)
    cy, cx = (ys.min() + ys.max()) / 2.0, (xs.min() + xs.max()) / 2.0
    r = min(ys.max() - ys.min(), xs.max() - xs.min()) / 6.0          # a third of the shorter side, as a radius
    layer = Image.new("1", (img.shape[1], img.shape[0]), 0)
    d = ImageDraw.Draw(layer)
    for arm in (0.0, np.pi):
        pts = []
        for i in range(129):
            t = i / 128.0
            a = arm + t * turns * 2 * np.pi
            pts.append((cx + t * r * np.cos(a), cy + t * r * np.sin(a)))
        d.line(pts, fill=1, width=width)
    on = np.asarray(layer, bool) & m
    img[on] = colour
    return on


def paint_atlas(x, labels, names, steps=4, blue=0.07, line=0.18, flat_radius=12, trim=True):
    """The whole atlas: every label island repainted, then the trim and the Shield's emblem. x is float 0..1."""
    import numpy as np
    out = x.copy()
    for i, name in enumerate(names, start=1):
        m = labels == i
        if m.sum() < 64:
            continue
        out[m] = paint_island(x, m, steps=steps, blue=blue, line=line, flat_radius=flat_radius)
    u8 = (out * 255.0 + 0.5).astype(np.uint8)
    for i, name in enumerate(names, start=1):
        m = labels == i
        if m.sum() < 64:
            continue
        if trim:
            draw_trim(u8, m)
        if name == "Shield":
            draw_spiral(u8, m)
    return u8


# ------------------------------------------------------------------------------------------------- masks (Blender)
def uv_labels(vehicles_dir, name, size=1024):
    """Rasterise each mesh object's UV0 triangles of <Name>/<Name>_LOD0.fbx into a uint8 label image (0 = empty)."""
    import bpy                                                      # Blender only
    import numpy as np
    from PIL import Image, ImageDraw
    bpy.ops.wm.read_factory_settings(use_empty=True)
    fbx = os.path.join(vehicles_dir, name, "%s_LOD0.fbx" % name)
    bpy.ops.import_scene.fbx(filepath=fbx)                          # read-only: nothing is ever exported
    meshes = [o for o in bpy.context.scene.objects if o.type == 'MESH']
    meshes.sort(key=lambda o: o.name)
    labels = np.zeros((size, size), np.uint8)
    out = []
    for o in meshes:
        me = o.data
        me.calc_loop_triangles()
        uv = me.uv_layers[0].data
        layer = Image.new("L", (size, size), 0)
        d = ImageDraw.Draw(layer)
        for t in me.loop_triangles:
            p = [(uv[li].uv[0] * (size - 1), (1.0 - uv[li].uv[1]) * (size - 1)) for li in t.loops]
            d.polygon(p, fill=255)
        m = np.asarray(layer, np.uint8) > 0
        out.append(o.name.split('.')[0])
        labels[m] = len(out)
    return labels, out


# ------------------------------------------------------------------------------------------------- main
def main(argv=None):
    ap = argparse.ArgumentParser(description="repaint a machine's atlas as flat toon paint")
    sub = ap.add_subparsers(dest="mode", required=True)
    p = sub.add_parser("masks"); p.add_argument("vehicles"); p.add_argument("name"); p.add_argument("out")
    p.add_argument("--size", type=int, default=1024)
    p = sub.add_parser("paint"); p.add_argument("atlas"); p.add_argument("masks")
    p.add_argument("--steps", type=int, default=4); p.add_argument("--blue", type=float, default=0.07)
    p.add_argument("--line", type=float, default=0.18); p.add_argument("--flat-radius", type=int, default=12)
    p = sub.add_parser("score"); p.add_argument("atlas", nargs="+")
    a = ap.parse_args(argv if argv is not None else _args())
    import numpy as np
    from PIL import Image
    if a.mode == "masks":
        labels, names = uv_labels(a.vehicles, a.name, a.size)
        np.savez(a.out, labels=labels, names=np.array(names))
        print("masks %s: %d parts (%s), %.1f %% of the atlas covered"
              % (a.out, len(names), ", ".join(names), 100.0 * (labels > 0).mean()))
        return 0
    if a.mode == "score":
        for f in a.atlas:
            x = np.asarray(Image.open(f).convert("RGB")).astype(np.float32) / 255.0
            print("swirl %.4f  %s" % (chroma_noise(x), f))
        return 0
    z = np.load(a.masks, allow_pickle=False)
    labels, names = z["labels"], [str(n) for n in z["names"]]
    x = np.asarray(Image.open(a.atlas).convert("RGB")).astype(np.float32) / 255.0
    before = chroma_noise(x)
    u8 = paint_atlas(x, labels, names, steps=a.steps, blue=a.blue, line=a.line, flat_radius=a.flat_radius)
    Image.fromarray(u8).save(a.atlas, quality=93)
    print("painted %s: %d islands, swirl %.4f -> %.4f" % (a.atlas, len(names), before,
                                                          chroma_noise(u8.astype(np.float32) / 255.0)))
    return 0


def _args():
    """Blender passes the script's own arguments after `--`."""
    v = sys.argv[1:]
    return v[v.index("--") + 1:] if "--" in v else v


if __name__ == "__main__":
    sys.exit(main())
