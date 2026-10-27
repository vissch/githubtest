"""Draw the nine ability-fx gas-bank chain folders into three sheets. Read-only: no capture, no Unity."""
import json
import os

import numpy as np
from PIL import Image, ImageDraw, ImageFont

HERE = os.path.dirname(os.path.abspath(__file__))
EVIDENCE = os.path.dirname(HERE)

CHAIN = [
    "ability-fx-captures-r2",
    "ability-fx-captures-r2-2-g2",
    "ability-fx-captures-r2-2-g2-g1",
    "ability-fx-captures-r2-2-g2-g1-g2",
    "ability-fx-captures-r2-2-g2-g1-g2-g2",
    "ability-fx-captures-r2-2-g2-g1-g2-g2-g2",
    "ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1",
    "ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g2",
    "ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g2-g1",
]
assert CHAIN == sorted(CHAIN, key=len)

KEYS = ["gas_green_share", "bank_paint_share", "bank_patch_count"]
DIRECTION = {"gas_green_share": "ge", "bank_paint_share": "ge", "bank_patch_count": "le"}

PANEL_W, PANEL_H = 640, 360
CAPTION_H = 120
CELL_W, CELL_H = PANEL_W, PANEL_H + CAPTION_H
GRID_COLS, GRID_ROWS = 3, 3
JPEG_QUALITY = 88


def short_name(folder):
    return folder[len("ability-fx-captures-"):]


def load_probe(folder):
    with open(os.path.join(EVIDENCE, folder, "probe.json"), "r", encoding="utf-8") as f:
        return json.load(f)


def step_run1_dir(folder):
    run1 = os.path.join(EVIDENCE, folder, "run1")
    if os.path.isdir(run1):
        return run1
    return os.path.join(EVIDENCE, folder)


def step_values(folder, probe):
    if "runs" in probe:
        vals = probe["runs"].get("run1", {})
        bars = probe.get("bars", {})
    else:
        vals = probe
        bars = {}
    out = {}
    for k in KEYS:
        v = vals.get(k, None)
        b = bars.get(k, None)
        out[k] = (v, b)
    return out


def get_font(size):
    try:
        return ImageFont.load_default(size=size)
    except TypeError:
        return None


def draw_caption(width, height, lines, scale3x_fallback):
    if scale3x_fallback:
        big = Image.new("RGB", (width * 3, height * 3), (20, 20, 20))
        draw = ImageDraw.Draw(big)
        font = ImageFont.load_default()
        y = 15
        for line, color in lines:
            draw.text((15, y), line, fill=color, font=font)
            y += 54
        return big.resize((width, height), Image.LANCZOS)
    img = Image.new("RGB", (width, height), (20, 20, 20))
    draw = ImageDraw.Draw(img)
    font = get_font(20)
    y = 6
    for line, color in lines:
        draw.text((6, y), line, fill=color, font=font)
        y += 24
    return img


def format_metric_lines(folder, values):
    lines = []
    for k in KEYS:
        v, b = values[k]
        if v is None:
            lines.append((f"{k}: n/a", (200, 200, 200)))
            continue
        vtxt = f"{v:.4f}" if isinstance(v, float) else str(v)
        if b is None:
            lines.append((f"{k}: {vtxt}", (230, 230, 230)))
            continue
        direction = DIRECTION[k]
        ok = (v >= b) if direction == "ge" else (v <= b)
        status = "pass" if ok else "FAIL"
        color = (90, 220, 90) if ok else (230, 70, 70)
        lines.append((f"{k}: {vtxt} ({'>=' if direction == 'ge' else '<='} {b}) {status}", color))
    return lines


def build_chain_sheet(image_name, out_path):
    sources = []
    cells = []
    for folder in CHAIN:
        probe = load_probe(folder)
        values = step_values(folder, probe)
        run_dir = step_run1_dir(folder)
        img_path = os.path.join(run_dir, image_name)
        if os.path.isfile(img_path):
            sources.append(img_path)
            panel = Image.open(img_path).convert("RGB").resize((PANEL_W, PANEL_H), Image.LANCZOS)
        else:
            print(f"WARNING: missing {img_path}, using a black panel")
            panel = Image.new("RGB", (PANEL_W, PANEL_H), (0, 0, 0))
        caption_lines = [(short_name(folder), (255, 255, 255))] + format_metric_lines(folder, values)
        caption = draw_caption(PANEL_W, CAPTION_H, caption_lines, scale3x_fallback=False)
        cell = Image.new("RGB", (CELL_W, CELL_H), (0, 0, 0))
        cell.paste(panel, (0, 0))
        cell.paste(caption, (0, PANEL_H))
        cells.append(cell)

    sheet = Image.new("RGB", (CELL_W * GRID_COLS, CELL_H * GRID_ROWS), (0, 0, 0))
    for i, cell in enumerate(cells):
        col = i % GRID_COLS
        row = i // GRID_COLS
        sheet.paste(cell, (col * CELL_W, row * CELL_H))
    sheet.save(out_path, quality=JPEG_QUALITY)
    return sources


def build_diff_sheet(out_path):
    newest = CHAIN[-1]
    run1 = step_run1_dir(newest)
    plate_path = os.path.join(run1, "gas-t1-plate.jpg")
    gas_path = os.path.join(run1, "gas-t1.jpg")
    sources = [plate_path, gas_path, gas_path]

    plate = Image.open(plate_path).convert("RGB").resize((PANEL_W, PANEL_H), Image.LANCZOS)
    gas = Image.open(gas_path).convert("RGB").resize((PANEL_W, PANEL_H), Image.LANCZOS)

    plate_arr = np.asarray(plate).astype(np.int16)
    gas_arr = np.asarray(gas).astype(np.int16)
    diff_arr = np.clip(np.abs(gas_arr - plate_arr) * 4, 0, 255).astype(np.uint8)
    diff = Image.fromarray(diff_arr, mode="RGB")

    panels = [
        (plate, "gas-t1-plate.jpg (gas off)"),
        (gas, "gas-t1.jpg (gas on)"),
        (diff, "|gas - plate| x 4"),
    ]

    cells = []
    for panel, caption in panels:
        caption_img = draw_caption(PANEL_W, 40, [(caption, (255, 255, 255))], scale3x_fallback=False)
        cell = Image.new("RGB", (CELL_W, PANEL_H + 40), (0, 0, 0))
        cell.paste(panel, (0, 0))
        cell.paste(caption_img, (0, PANEL_H))
        cells.append(cell)

    sheet = Image.new("RGB", (CELL_W * 3, PANEL_H + 40), (0, 0, 0))
    for i, cell in enumerate(cells):
        sheet.paste(cell, (i * CELL_W, 0))
    sheet.save(out_path, quality=JPEG_QUALITY)
    return sources


def main():
    t1_sources = build_chain_sheet("gas-t1.jpg", os.path.join(HERE, "chain-t1.jpg"))
    o240_sources = build_chain_sheet("gas-o240.jpg", os.path.join(HERE, "chain-o240.jpg"))
    diff_sources = build_diff_sheet(os.path.join(HERE, "diff-now.jpg"))

    for label, sources in (("T1", t1_sources), ("O240", o240_sources), ("diff", diff_sources)):
        for s in sources:
            print(f"{label} source: {s}")

    written = []
    for name in ("chain-t1.jpg", "chain-o240.jpg", "diff-now.jpg"):
        path = os.path.join(HERE, name)
        if os.path.isfile(path):
            written.append(path)
    for p in written:
        print(f"wrote: {p}")

    if len(written) != 3:
        raise SystemExit(1)


if __name__ == "__main__":
    main()
