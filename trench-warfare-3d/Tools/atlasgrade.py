"""Grade a machine's atlas so it sits with the others at night: darken by a gamma, lift saturation, and tint its near-greys.
A Tripo sculpt painted in pale steel (the Bullfrog: median luminance 0.37 against the Brute's 0.24 and the Croaker's 0.20)
is the brightest thing on a night field and reads as unlit. Idempotent only from the splitter's own output: run it once,
after mechsplit.py, on <Name>Atlas.jpg (the split rewrites the atlas, so a re-split needs it again).

usage: python Tools/atlasgrade.py <atlas.jpg> [--gamma 1.4] [--sat 1.25] [--tint 0.84,0.98,0.92] [--grey 0.18]
  --tint multiplies pixels whose saturation is under --grey (blending out to none at twice that), so paint keeps its hue.
Prints the median luminance before and after."""
import sys, argparse
import numpy as np
from PIL import Image

ap = argparse.ArgumentParser()
ap.add_argument("atlas"); ap.add_argument("--gamma", type=float, default=1.4); ap.add_argument("--sat", type=float, default=1.25)
ap.add_argument("--tint", default="0.84,0.98,0.92"); ap.add_argument("--grey", type=float, default=0.18)
a = ap.parse_args()
im = Image.open(a.atlas).convert("RGB")
x = np.asarray(im).astype(np.float32) / 255.0
before = float(np.median(x.mean(2)))
x = np.power(x, a.gamma)
lum = x.mean(2, keepdims=True)
mx, mn = x.max(2, keepdims=True), x.min(2, keepdims=True)
sat = np.where(mx > 1e-4, (mx - mn) / np.maximum(mx, 1e-4), 0.0)
x = np.clip(lum + (x - lum) * a.sat, 0.0, 1.0)
tint = np.array([float(v) for v in a.tint.split(",")], dtype=np.float32).reshape(1, 1, 3)
w = np.clip(1.0 - (sat - a.grey) / a.grey, 0.0, 1.0)          # 1 on a grey, 0 on paint
x = np.clip(x * (1.0 + (tint - 1.0) * w), 0.0, 1.0)
Image.fromarray((x * 255.0 + 0.5).astype(np.uint8)).save(a.atlas, quality=93)
print("graded %s: median luminance %.3f -> %.3f" % (a.atlas, before, float(np.median(x.mean(2)))))
