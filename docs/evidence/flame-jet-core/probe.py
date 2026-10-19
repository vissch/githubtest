"""Measure the flamethrower jet's hot core from the pixels of the T2 night stills beside this file.

The owner's complaint (2026-10-19, harsh critic 31/100) was that the jet is "flat see-through orange cut-outs"
and that he wants "an opaque white-yellow core over the first third, orange towards the tip, dark smoke past the
tip, nothing showing through near the nozzle". Both halves of that are measurable, so neither is argued about:

  core_over_tip_luma  the mean luminance of the fire over the first third of the stream, over the mean over its
                      last sixth. Above 1 the mouth out-burns the tip, which is the reversal the owner asked for.
  see_through_frac    of the fire's pixels in that near third, the share that still show the EMPTY FIELD behind
                      them unchanged (within 6/255 per channel of bg-t2-after.png, the same pose with no fire).
                      This is "nothing showing through near the nozzle" as a number.

The aim points screen-right by construction (the capture frames at yaw 0 and fires along the camera's flattened
right), so the nozzle is the fire mask's left edge and the tip its right.

The mask is flamecheck.py's own rule, so the two tools agree about what is fire. Writes probe.json beside itself;
nothing in that file is typed by hand.  usage: python docs/evidence/flame-jet-core/probe.py
"""
import json, os, sys
import numpy as np
from PIL import Image
from scipy import ndimage

HERE = os.path.dirname(os.path.abspath(__file__))
JET = os.path.join(HERE, "jet-t2-after.png")
BG = os.path.join(HERE, "bg-t2-after.png")


def rgb(path):
    return np.asarray(Image.open(path).convert("RGB")).astype(np.float32)


def luma(x):
    return 0.299 * x[..., 0] + 0.587 * x[..., 1] + 0.114 * x[..., 2]


def firemask(x):
    lum, rb = luma(x), x[..., 0] - x[..., 2]
    m = ((rb >= 60) & (lum >= 70)) | ((lum >= 190) & (rb >= 15))
    m = ndimage.binary_closing(ndimage.binary_opening(m, np.ones((3, 3))), np.ones((5, 5)))
    # the largest connected piece only: the pool of burning fuel on the ground and the lick thrown clear of the
    # head are fire too, and they are not the stream whose core is being measured.
    lab, n = ndimage.label(m)
    if n == 0:
        return m
    sizes = ndimage.sum(m, lab, range(1, n + 1))
    return lab == (int(np.argmax(sizes)) + 1)


def main():
    jet, bg = rgb(JET), rgb(BG)
    if jet.shape != bg.shape:
        print("the jet still and the background plate are different sizes", file=sys.stderr)
        return 1
    m = firemask(jet)
    if m.sum() < 15000:
        print("no fire in %s - do not measure this capture" % JET, file=sys.stderr)
        return 1
    xs = np.where(m.any(axis=0))[0]
    x0, x1 = int(xs[0]), int(xs[-1])
    span = x1 - x0 + 1
    lum = luma(jet)
    near = m.copy(); near[:, x0 + span // 3:] = False                      # the first third: the nozzle end
    tip = m.copy(); tip[:, : x1 - max(1, span // 6)] = False               # the last sixth: the tip
    core = float(lum[near].mean()) / max(1e-6, float(lum[tip].mean()))
    # see-through: a near-third fire pixel that still matches the empty field behind it to within 6/255 per channel
    same = (np.abs(jet - bg) <= 6.0).all(axis=2)
    see = float(same[near].sum()) / max(1, int(near.sum()))
    print("stream x %d..%d (%d px)   near third %d px   tip sixth %d px" % (x0, x1, span, near.sum(), tip.sum()))
    print("core_over_tip_luma = %.3f   (want >= 1.5)" % core)
    print("see_through_frac   = %.4f   (want < 0.1)" % see)
    out = {"core_over_tip_luma": round(core, 4), "see_through_frac": round(see, 4)}
    with open(os.path.join(HERE, "probe.json"), "w", encoding="utf-8") as f:
        json.dump(out, f, indent=2)
        f.write("\n")
    return 0 if (core >= 1.5 and see < 0.1) else 1


if __name__ == "__main__":
    sys.exit(main())
