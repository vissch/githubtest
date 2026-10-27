#!/usr/bin/env python3
"""Draw the ability-FX gas chain's own committed stills onto three sheets a person can look at.

Read-only over the evidence folders: it renders nothing and re-measures nothing. Every number comes from
the step folder's own probe.json, with that folder's own bar beside it.

    python docs/evidence/ability-fx-captures-visual-review-3/sheet.py
"""
import json
import os
import sys

import numpy as np
from PIL import Image, ImageDraw, ImageFont

HERE = os.path.dirname(os.path.abspath(__file__))
EVIDENCE = os.path.dirname(HERE)
PREFIX = "ability-fx-captures-"
# the three keys the sheet shows, with the bar that guards each one in the newest folder.
# a key a folder's probe never had prints a dash: never map an old key onto a new one.
KEYS = [
    ("green_low_share", "green_low_share", "ge"),
    ("t1_visible_clouds", None, None),
    ("o240_visible_clouds", "o240_visible_floor", "ge"),
]
COLS, ROWS = 4, 3
PANEL_W, PANEL_H = 640, 360
CAP_H = 110
QUALITY = 88


def font(size):
    try:
        return ImageFont.load_default(size=size), 1
    except TypeError:
        return ImageFont.load_default(), 3


def steps():
    """The chain folders, sorted by name length so a fix unit never sorts before its parent."""
    names = [n for n in os.listdir(EVIDENCE)
             if n.startswith(PREFIX) and os.path.isdir(os.path.join(EVIDENCE, n))
             and os.path.exists(os.path.join(EVIDENCE, n, "probe.json"))]
    return sorted(names, key=len)


def still(name, base):
    """run1/<base> when the folder has a run1/, else the folder's own <base> (only the oldest step)."""
    d = os.path.join(EVIDENCE, name)
    for p in (os.path.join(d, "run1", base), os.path.join(d, base)):
        if os.path.exists(p):
            return p
    return None


def readings(name):
    d = json.load(open(os.path.join(EVIDENCE, name, "probe.json"), encoding="utf-8"))
    src = d.get("runs", {}).get("run1") or d
    bars = d.get("bars", {})
    out = []
    for key, bar_key, cmp_ in KEYS:
        if key not in src:
            out.append("%s  (not measured yet)" % key)
            continue
        val = src[key]
        txt = "%s  %s" % (key, ("%.4f" % val) if isinstance(val, float) else val)
        if bar_key and bar_key in bars:
            bar = bars[bar_key]
            ok = val >= bar if cmp_ == "ge" else (val <= bar if cmp_ == "le" else val < bar)
            txt += "   [%s %s %s: %s]" % (bar_key, cmp_, bar, "pass" if ok else "FAIL")
        out.append(txt)
    return out


def short(name):
    return name[len(PREFIX):].replace("r2-2-", "", 1)


def cell(img, title, lines, fnt, scale):
    """One captioned panel: the still on top, the caption strip under it."""
    out = Image.new("RGB", (PANEL_W, PANEL_H + CAP_H), (0, 0, 0))
    if img is not None:
        out.paste(img.convert("RGB").resize((PANEL_W, PANEL_H), Image.LANCZOS), (0, 0))
    strip = Image.new("RGB", (PANEL_W * scale, CAP_H * scale), (24, 24, 24))
    d = ImageDraw.Draw(strip)
    d.text((8 * scale, 6 * scale), title, font=fnt, fill=(255, 255, 160))
    for i, line in enumerate(lines):
        colour = (255, 140, 140) if "FAIL" in line else (220, 220, 220)
        d.text((8 * scale, (30 + 24 * i) * scale), line, font=fnt, fill=colour)
    if scale != 1:
        strip = strip.resize((PANEL_W, CAP_H), Image.LANCZOS)
    out.paste(strip, (0, PANEL_H))
    return out


def grid(cells):
    w, h = PANEL_W, PANEL_H + CAP_H
    sheet = Image.new("RGB", (COLS * w, ROWS * h), (0, 0, 0))
    for i, c in enumerate(cells):
        sheet.paste(c, ((i % COLS) * w, (i // COLS) * h))
    return sheet


def chain_sheet(names, base, fnt, scale, read):
    cells = []
    for i, name in enumerate(names):
        p = still(name, base)
        read.append(p or "MISSING %s/%s" % (name, base))
        img = Image.open(p) if p else None
        cells.append(cell(img, "%d. %s" % (i + 1, short(name)), readings(name), fnt, scale))
    return grid(cells)


def diff_sheet(newest, fnt, scale, read):
    srcs = [still(newest, "gas-t1-plate.jpg"), still(newest, "gas-t1.jpg")]
    caps = ["gas off (plate): gas-t1-plate.jpg", "gas on: gas-t1.jpg", "|gas - plate| x 4, clipped"]
    if any(s is None for s in srcs):
        raise SystemExit("diff-now.jpg: missing %s" % srcs)
    read.extend(srcs)
    plate, gas = (Image.open(s).convert("RGB") for s in srcs)
    diff = np.clip(np.abs(np.asarray(gas, np.int16) - np.asarray(plate, np.int16)) * 4, 0, 255)
    imgs = [plate, gas, Image.fromarray(diff.astype(np.uint8))]
    read.append("(computed) |gas - plate| x 4")
    cells = [cell(im, caps[i], ["from " + newest], fnt, scale) for i, im in enumerate(imgs)]
    w, h = PANEL_W, PANEL_H + CAP_H
    sheet = Image.new("RGB", (3 * w, h), (0, 0, 0))
    for i, c in enumerate(cells):
        sheet.paste(c, (i * w, 0))
    return sheet


def main():
    fnt, scale = font(20)
    names = steps()
    if len(names) > COLS * ROWS:
        raise SystemExit("%d chain folders do not fit a %dx%d grid" % (len(names), COLS, ROWS))
    written = []
    for base, out in (("gas-t1.jpg", "chain-t1.jpg"), ("gas-o240.jpg", "chain-o240.jpg")):
        read = []
        sheet = chain_sheet(names, base, fnt, scale, read)
        print("%s sources (%d):" % (out, len(read)))
        for p in read:
            print("   ", p)
        sheet.save(os.path.join(HERE, out), quality=QUALITY)
        written.append(out)
    read = []
    sheet = diff_sheet(names[-1], fnt, scale, read)
    print("diff-now.jpg sources (%d):" % len(read))
    for p in read:
        print("   ", p)
    sheet.save(os.path.join(HERE, "diff-now.jpg"), quality=QUALITY)
    written.append("diff-now.jpg")
    for w in written:
        print("wrote", os.path.join(HERE, w))
    return 0 if len(written) == 3 else 1


if __name__ == "__main__":
    sys.exit(main())
