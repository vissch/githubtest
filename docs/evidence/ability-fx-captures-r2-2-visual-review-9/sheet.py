#!/usr/bin/env python3
"""Contact sheets of the ability-fx-captures-r2-2 chain, for the owner to look at, not read as numbers.

WHY. Nine fix units each shot three keeper captures and committed them under docs/evidence/. Nobody put them in
front of the owner: he has read nine tables of numbers (green_low_share, t1_visible_clouds, ...) and never seen
the frame they describe. This tiles the captures already committed in the chain into three PNGs he can open.

No capture is re-rendered here and no look, constant or bar is touched: this only reads jpgs and probe.json
already on disk and draws them into a grid.

Grid arithmetic (worked by hand, checked by --selftest):
    each still is 1280x720; at a third size that is 1280/3 = 426 wide, 720/3 = 240 tall.
    a 24 px label band under it makes each cell 426x264.
    three columns: 3 * 426 = 1278 wide.
    nine panels (3 rows of 3): 3 * 264 = 792 tall  -> sheet-t1.png, sheet-o240.png are 1278x792.
    five panels (2 rows of 3, one empty): 2 * 264 = 528 tall -> sheet-cards.png is 1278x528.

    python sheet.py              build sheet-t1.png, sheet-o240.png, sheet-cards.png here
    python sheet.py --selftest   the two hand-worked grid cases above, no capture read
"""
import glob
import json
import os
import sys

from PIL import Image, ImageDraw, ImageFont

HERE = os.path.dirname(os.path.abspath(__file__))
EVIDENCE_DIR = os.path.dirname(HERE)

CELL_W, CELL_H = 426, 240
LABEL_H = 24
COLS = 3


def _font():
    try:
        return ImageFont.truetype("arial.ttf", 13)
    except Exception:
        return ImageFont.load_default()


def _chain_sort_key(folder_name):
    # oldest unit first: shorter -g suffix sorts before longer ones.
    return len(folder_name)


def _probe_value(probe, *paths):
    for path in paths:
        node = probe
        ok = True
        for key in path:
            if isinstance(node, dict) and key in node:
                node = node[key]
            else:
                ok = False
                break
        if ok and node is not None:
            return node
    return None


def _label_for(folder):
    short = os.path.basename(folder).replace("ability-fx-captures-", "")
    probe_path = os.path.join(folder, "probe.json")
    clouds = "-"
    low_share = "-"
    if os.path.isfile(probe_path):
        with open(probe_path, "r", encoding="utf-8") as f:
            probe = json.load(f)
        run1 = _probe_value(probe, ("runs", "run1")) or {}
        clouds_val = _probe_value(run1, ("t1_visible_clouds",), ("t1_cloud_count",))
        low_val = _probe_value(run1, ("green_low_share",), ("bank_low_share",))
        if clouds_val is not None:
            clouds = str(clouds_val)
        if low_val is not None:
            low_share = "%.4g" % low_val if isinstance(low_val, float) else str(low_val)
    tag = ""
    if not os.path.isdir(os.path.join(folder, "run1")) and os.path.isdir(os.path.join(folder, "run-today")):
        tag = " HEAD not landed"   # the skirt breaks the floor; this is not the look of the landed bank
    return "%s  clouds=%s low=%s%s" % (short, clouds, low_share, tag)


def _tile(panels, cols=COLS):
    cell_h = CELL_H + LABEL_H
    rows = (len(panels) + cols - 1) // cols
    sheet = Image.new("RGB", (cols * CELL_W, rows * cell_h), "white")
    draw = ImageDraw.Draw(sheet)
    font = _font()
    for i, (img, label) in enumerate(panels):
        col = i % cols
        row = i // cols
        x = col * CELL_W
        y = row * cell_h
        thumb = img.resize((CELL_W, CELL_H))
        sheet.paste(thumb, (x, y))
        draw.text((x + 4, y + CELL_H + 5), label, fill="black", font=font)
    return sheet


def _ordered_chain_folders():
    folders = [
        d for d in glob.glob(os.path.join(EVIDENCE_DIR, "*"))
        if os.path.isdir(d) and os.path.basename(d) != os.path.basename(HERE)
    ]
    return sorted(folders, key=lambda d: _chain_sort_key(os.path.basename(d)))


def build_run_sheet(still_name, out_name):
    panels = []
    for folder in _ordered_chain_folders():
        jpg = os.path.join(folder, "run1", still_name)
        if not os.path.isfile(jpg):
            jpg = os.path.join(folder, "run-today", still_name)   # HEAD's own fresh shot, no keeper run yet
        if not os.path.isfile(jpg):
            continue
        img = Image.open(jpg).convert("RGB")
        panels.append((img, _label_for(folder)))
    sheet = _tile(panels)
    sheet.save(os.path.join(HERE, out_name))
    return len(panels)


def _newest_cards_folder():
    for folder in reversed(_ordered_chain_folders()):
        if os.path.isdir(os.path.join(folder, "cards")):
            return folder
    return None


def build_cards_sheet(out_name="sheet-cards.png"):
    folder = _newest_cards_folder()
    if folder is None:
        sheet = _tile([])
        sheet.save(os.path.join(HERE, out_name))
        return 0
    label_base = os.path.basename(folder).replace("ability-fx-captures-", "")
    panels = []
    for stem in ("gas", "gaspuff", "puff", "smoke", "plate"):
        jpg = os.path.join(folder, "cards", "cards-%s.jpg" % stem)
        if not os.path.isfile(jpg):
            continue
        img = Image.open(jpg).convert("RGB")
        panels.append((img, "%s %s" % (label_base, stem)))
    sheet = _tile(panels)
    sheet.save(os.path.join(HERE, out_name))
    return len(panels)


def selftest():
    nine = [(Image.new("RGB", (1280, 720), "red"), "x") for _ in range(9)]
    sheet9 = _tile(nine)
    assert sheet9.size == (1278, 792), sheet9.size

    five = [(Image.new("RGB", (1280, 720), "blue"), "x") for _ in range(5)]
    sheet5 = _tile(five)
    assert sheet5.size == (1278, 528), sheet5.size

    missing_label = _label_for(os.path.join(EVIDENCE_DIR, "no-such-folder-at-all"))
    assert "clouds=-" in missing_label and "low=-" in missing_label, missing_label

    print("selftest OK: 9 panels -> %s, 5 panels -> %s, missing probe.json -> dash" % (sheet9.size, sheet5.size))
    return 0


def main():
    if "--selftest" in sys.argv:
        return selftest()
    n_t1 = build_run_sheet("gas-t1.jpg", "sheet-t1.png")
    n_o240 = build_run_sheet("gas-o240.jpg", "sheet-o240.png")
    n_cards = build_cards_sheet()
    print("%d panels for t1, %d for o240, %d for cards" % (n_t1, n_o240, n_cards))
    return 0


if __name__ == "__main__":
    sys.exit(main())
