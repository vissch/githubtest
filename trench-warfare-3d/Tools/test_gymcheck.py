#!/usr/bin/env python3
"""Tools/gymcheck.py's own tests. Hermetic: every picture is drawn here with PIL, nothing is read off disk.

Run by Tools/toolcheck.py (which finds it by name) as a plain script; the exit code is the verdict.

The cases are the ways the measure has to behave:
  a mud frame with a cyan bracket on it     RING  - round 3's fault, and what the check exists to catch
  a SMALL cyan bracket in a wide frame      RING  - Banner's case: 0.07 % of the frame, which the first draft's
                                                    0.5 % limit passed. This is why the limit is 0.02 %.
  the same frame with no cyan at all        CLEAR
  a frame of dull, dark cyan-ish shadow     CLEAR - saturation and value, not hue alone
  a frame of SKY blue                       CLEAR - the hue window stops at 200 degrees
"""
import os
import sys
import tempfile

from PIL import Image, ImageDraw

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from gymcheck import cyan_share, MAX_SHARE   # noqa: E402

W, H = 800, 450
MUD = (92, 74, 58)
RING = (89, 217, 255)         # TankRenderer.TeamA (0.35, 0.85, 1.00) in bytes
DULL = (30, 48, 52)           # the same hue, dark: a cyan-tinted shadow
SKY = (120, 150, 235)         # past 200 degrees, and that is where the sky lives
failures = []


def share(draw_it):
    img = Image.new("RGB", (W, H), MUD)
    draw_it(ImageDraw.Draw(img))
    path = os.path.join(tempfile.gettempdir(), "gymcheck_case.png")
    img.save(path)
    try:
        return cyan_share(path)[0]
    finally:
        try:
            os.remove(path)
        except OSError:
            pass


def case(name, drawn, want_ring):
    s = share(drawn)
    got = s > MAX_SHARE
    verdict = "RING" if got else "CLEAR"
    print("%-34s cyan=%.4f -> %-5s (want %s)" % (name, s, verdict, "RING" if want_ring else "CLEAR"))
    if got != want_ring:
        failures.append(name)


def bracket(d, x0, y0, x1, y1, w):
    """The ring as the gym draws it: a thick open bracket lying on the ground under the hull."""
    d.line([(x0, y0), (x0, y1), (x1, y1), (x1, y0)], fill=RING, width=w)


case("a bracket across a close hull", lambda d: bracket(d, 120, 180, 680, 380, 14), True)
case("a small bracket in a wide frame", lambda d: bracket(d, 360, 230, 440, 265, 3), True)
case("nothing but mud", lambda d: None, False)
case("a cyan-tinted shadow over it all", lambda d: d.rectangle([0, 0, W, H], fill=DULL), False)
case("a sky-blue sky over half of it", lambda d: d.rectangle([0, 0, W, H // 2], fill=SKY), False)

if failures:
    print("FAIL: " + ", ".join(failures))
    sys.exit(1)
print("gymcheck: all cases pass")
