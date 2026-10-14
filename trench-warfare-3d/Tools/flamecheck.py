"""Does this capture actually contain fire?  usage: python Tools/flamecheck.py <png> [minpx]

A whole critique round was spent on a wall shot that contained no flame at all: the rig had framed a
slope the stream never reached, the capture succeeded, and the numbers that came back described an
empty field. A shot that cost forty seconds to set up is worth one second of checking.
"""
import sys
# scipy and numpy are not in every environment. Exit 2 (not 1) when they are missing, so a caller can tell
# "the check could not run" from "this capture contains no fire" - reporting the first as the second sent a
# whole critique round after an effect that was fine.
try:
    import numpy as np
    from PIL import Image
    from scipy import ndimage
except ImportError as e:
    sys.stderr.write("flamecheck: cannot run, missing %s (needs numpy, pillow, scipy)\n" % e.name)
    sys.exit(2)

p = sys.argv[1]
floor = int(sys.argv[2]) if len(sys.argv) > 2 else 15000
x = np.asarray(Image.open(p).convert("RGB")).astype(np.float32)
lum = 0.299*x[..., 0] + 0.587*x[..., 1] + 0.114*x[..., 2]
rb = x[..., 0] - x[..., 2]
fire = ((rb >= 60) & (lum >= 70)) | ((lum >= 190) & (rb >= 15))
fire = ndimage.binary_closing(ndimage.binary_opening(fire, np.ones((3, 3))), np.ones((5, 5)))
n = int(fire.sum())
ok = n >= floor
print("%s fire=%d px (floor %d) %s" % ("OK  " if ok else "EMPTY", n, floor, p))
sys.exit(0 if ok else 1)
