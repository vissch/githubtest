#!/usr/bin/env python3
"""loop.py STEM [MS]: join a bang shot's frames into a loop, so motion can be judged, not poses.

A shot taken on the bang (docs/22) leaves five frames named STEM_f1.png .. STEM_f5.png around the event. This joins
them into STEM.gif, each frame MS milliseconds (400 by default), looping forever: five frames are a two-second loop.
The recipe is seq.sh's inline Pillow block, pulled out so bang.sh and a person can both call it.
"""
import glob
import sys


def main(argv):
    if not argv:
        print("usage: loop.py STEM [MS]")
        return 2
    stem = argv[0]
    ms = int(argv[1]) if len(argv) > 1 else 400
    fs = sorted(glob.glob(stem + "_f*.png"))
    if len(fs) < 2:
        print("LOOP %s: no frames" % stem)
        return 1
    from PIL import Image
    ims = [Image.open(f).convert("RGB").quantize(128) for f in fs]
    gif = stem + ".gif"
    ims[0].save(gif, save_all=True, append_images=ims[1:], duration=ms, loop=0)
    print("LOOP %s: %d frames, %d ms each -> %s" % (stem, len(ims), ms, gif))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
