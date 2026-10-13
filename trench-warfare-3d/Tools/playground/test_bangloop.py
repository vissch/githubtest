#!/usr/bin/env python3
"""test_bangloop.py: loop.py turns a bang shot's five frames into a two-second loop.

The goal's words: five frames around the event, joined into a two-second loop. So 400 ms x 5 = 2000 ms, looping
forever. Also: fewer than two frames is an error, not an empty gif.
"""
import os
import subprocess
import sys
import tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
LOOP = os.path.join(HERE, "loop.py")


def run(stem, *args):
    return subprocess.run([sys.executable, LOOP, stem] + [str(a) for a in args],
                          capture_output=True, text=True)


def frames(d, stem, n):
    from PIL import Image
    for k in range(1, n + 1):
        Image.new("RGB", (8, 8), (k * 20, 0, 0)).save(os.path.join(d, "%s_f%d.png" % (stem, k)))
    return os.path.join(d, stem)


def main():
    try:
        from PIL import Image  # noqa: F401
    except ImportError:
        print("skipped (no Pillow)")
        return 0
    from PIL import Image

    with tempfile.TemporaryDirectory() as d:
        stem = frames(d, "demo_hit", 5)
        r = run(stem)
        assert r.returncode == 0, "five frames should join: %s%s" % (r.stdout, r.stderr)
        gif = stem + ".gif"
        assert os.path.isfile(gif), "no gif at " + gif
        with Image.open(gif) as im:
            assert im.n_frames == 5, "frames: %d" % im.n_frames
            total = 0
            for k in range(im.n_frames):
                im.seek(k)
                dur = im.info["duration"]
                assert dur == 400, "frame %d lasts %s ms, not 400" % (k, dur)
                total += dur
            assert total == 2000, "the loop is %d ms, not the goal's two seconds" % total
            im.seek(0)
            assert im.info.get("loop") == 0, "loop: %r" % im.info.get("loop")

        one = frames(d, "lonely", 1)
        r = run(one)
        assert r.returncode == 1, "one frame is not a loop: exit %d" % r.returncode
        assert "no frames" in r.stdout, r.stdout

    print("bangloop ok")
    return 0


if __name__ == "__main__":
    sys.exit(main())
