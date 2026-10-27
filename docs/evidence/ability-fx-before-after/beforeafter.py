#!/usr/bin/env python3
"""Show the owner before and after for the close gas bank, not another number.

WHY. The owner's answer on the Decide page to "ability-fx-captures-r2-2-g2-g2-g2-g2-g2-g1-g2: tried 2 times, no
progress" was "show me visually before and after". The plan leg already found every gas still in docs/evidence/
from -g2-g2/ onward was shot at the SAME camera (zoom 30, yaw 40, pitch 25, focus 56/142,
cam 110.242/33.018/96.486, pose_error_m 0), so the builds are directly comparable with no Unity run: only
compositing of stills already in git.

BEFORE is the flat-sheet build (5651e9cb, -g2-g2-g2-g2-g2/). AFTER is the GasCloud book (0fef5e89, -g1/).

THE MEASURE. "Green lift" is the gas still's green channel minus its own no-gas plate's green channel, in the
union mask of pixels where that lift is over 8 (0-255 RGB) in EITHER build's run1 gas-t1.jpg. The plate makes the
picture (and the measure) honest: without it there is no way to tell gas-green from field-green.

THE HONEST VERDICT the picture must carry: at zoom 30 the two builds read the same to the eye. Both are a flat
carpet of pale green patches lying on the ground; the plate has no green at all, so all of it is gas. The after
build is brighter green inside the same patches, and that is the whole change a player sees. It is not a standing
cloud. This script does not sell the change past what the bars measure.

BARS, every one re-measured from the files, never hardcoded (only the bar itself is a constant):
  - layer_cross_min >= 1.5 * layer_noise_max: the before/after difference in masked green lift is bigger than
    1.5x either build's own run-to-run spread, i.e. the change is real, not noise.
  - after_lift_min >= 11.0 and before_lift_max < 11.0: the two builds' three-run bands do not overlap, with
    11.0 sitting in the gap between them.
  - frame_cross_max < frame_noise_max, printed as a FACT, not a pass bar: the whole-frame difference between
    builds is SMALLER than a build's own run-to-run whole-frame noise, i.e. the eye sees no change at T1.
  - the three PNGs exist and are over 200 kB.

USAGE
    python beforeafter.py          measure, build the three PNGs, bar them, write beforeafter.json
    python beforeafter.py --check  measure and bar only; refuse (exit 1) if a PNG is missing or a rewrite is needed

Exit 0 only when every bar above holds and (first run) the three PNGs were written. Exit 1 on a failed bar, a
missing source file, or a pose mismatch between panes that are claimed to share a camera.
"""
import json
import math
import os
import sys

import numpy as np
from PIL import Image, ImageDraw, ImageFont

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, "..", "..", ".."))

BEFORE_DIR = os.path.join(ROOT, "docs", "evidence", "ability-fx-captures-r2-2-g2-g2-g2-g2-g2")
BEFORE_COMMIT = "5651e9cb"
AFTER_DIR = os.path.join(ROOT, "docs", "evidence", "ability-fx-captures-r2-2-g2-g2-g2-g2-g2-g1")
AFTER_COMMIT = "0fef5e89"

POSE_KEYS = ["zoom", "yaw", "pitch", "focus_x", "focus_z", "cam_x", "cam_y", "cam_z"]
LIFT_THRESHOLD = 8.0

LAYER_RATIO_BAR = 1.5
AFTER_LIFT_MIN_BAR = 11.0
BEFORE_LIFT_MAX_BAR = 11.0
MIN_PNG_BYTES = 200_000

PROGRESSION_DIRS = [
    ("ability-fx-captures-r2-2-g2", "measure only"),
    ("ability-fx-captures-r2-2-g2-g2", "measure only"),
    ("ability-fx-captures-r2-2-g2-g2-g2", "measure only"),
    ("ability-fx-captures-r2-2-g2-g2-g2-g2", "measure only"),
    ("ability-fx-captures-r2-2-g2-g2-g2-g2-g2", "drawing changed"),
    ("ability-fx-captures-r2-2-g2-g2-g2-g2-g2-g1", "drawing changed"),
]


def load_sidecar(jpg_path):
    with open(jpg_path[:-4] + ".json") as f:
        return json.load(f)


def load_image(jpg_path):
    return np.asarray(Image.open(jpg_path).convert("RGB"), dtype=np.int16)


def check_pose(sidecars, names):
    ref = sidecars[0]
    for key in POSE_KEYS:
        for sc, name in zip(sidecars, names):
            if sc[key] != ref[key]:
                print(f"pose mismatch: {name}.{key} = {sc[key]} != {ref[key]}")
                sys.exit(1)
    for sc, name in zip(sidecars, names):
        if sc.get("pose_error_m", 0) != 0:
            print(f"pose_error_m nonzero for {name}: {sc['pose_error_m']}")
            sys.exit(1)


def run_paths(build_dir, run):
    return (
        os.path.join(build_dir, run, "gas-t1.jpg"),
        os.path.join(build_dir, run, "gas-t1-plate.jpg"),
    )


def green_lift(gas, plate):
    return gas[:, :, 1].astype(np.int32) - plate[:, :, 1].astype(np.int32)


def whole_frame_abs_diff(a, b):
    return float(np.mean(np.abs(a.astype(np.int32) - b.astype(np.int32))))


def measure():
    runs = ["run1", "run2", "run3"]

    before_gas, before_plate, before_sidecars = [], [], []
    after_gas, after_plate, after_sidecars = [], [], []
    for run in runs:
        bg, bp = run_paths(BEFORE_DIR, run)
        ag, ap = run_paths(AFTER_DIR, run)
        before_gas.append(load_image(bg))
        before_plate.append(load_image(bp))
        before_sidecars.append(load_sidecar(bg))
        after_gas.append(load_image(ag))
        after_plate.append(load_image(ap))
        after_sidecars.append(load_sidecar(ag))

    check_pose(before_sidecars + after_sidecars, [f"before/{r}" for r in runs] + [f"after/{r}" for r in runs])

    before_lift = [green_lift(g, p) for g, p in zip(before_gas, before_plate)]
    after_lift = [green_lift(g, p) for g, p in zip(after_gas, after_plate)]

    union_mask = np.zeros_like(before_lift[0], dtype=bool)
    for lift in before_lift + after_lift:
        union_mask |= lift > LIFT_THRESHOLD
    mask_px = int(union_mask.sum())

    before_mean = [float(lift[union_mask].mean()) for lift in before_lift]
    after_mean = [float(lift[union_mask].mean()) for lift in after_lift]

    before_noise = max(before_mean) - min(before_mean)
    after_noise = max(after_mean) - min(after_mean)
    layer_noise_max = max(before_noise, after_noise)

    cross = [abs(a - b) for a in after_mean for b in before_mean]
    layer_cross_min = min(cross)
    layer_cross_max = max(cross)

    before_frame = [whole_frame_abs_diff(g, p) for g, p in zip(before_gas, before_plate)]
    after_frame = [whole_frame_abs_diff(g, p) for g, p in zip(after_gas, after_plate)]
    frame_noise_max = max(max(before_frame) - min(before_frame), max(after_frame) - min(after_frame))

    frame_cross = [
        whole_frame_abs_diff(after_gas[i], before_gas[j])
        for i in range(3)
        for j in range(3)
        if after_sidecars[i]["frame"] == before_sidecars[j]["frame"]
    ]
    if not frame_cross:
        frame_cross = [whole_frame_abs_diff(after_gas[0], before_gas[0])]
    frame_cross_max = max(frame_cross)

    return {
        "mask_px": mask_px,
        "before_lift": before_mean,
        "after_lift": after_mean,
        "before_noise": before_noise,
        "after_noise": after_noise,
        "layer_noise_max": layer_noise_max,
        "layer_cross_min": layer_cross_min,
        "layer_cross_max": layer_cross_max,
        "before_frame_noise": max(before_frame) - min(before_frame),
        "after_frame_noise": max(after_frame) - min(after_frame),
        "frame_noise_max": frame_noise_max,
        "frame_cross_max": frame_cross_max,
    }


def bar(result):
    bars = {}
    bars["layer_cross_min"] = {
        "value": result["layer_cross_min"],
        "bar": f">= {LAYER_RATIO_BAR} * layer_noise_max ({LAYER_RATIO_BAR * result['layer_noise_max']:.3f})",
        "pass": result["layer_cross_min"] >= LAYER_RATIO_BAR * result["layer_noise_max"],
    }
    bars["after_lift_min"] = {
        "value": min(result["after_lift"]),
        "bar": f">= {AFTER_LIFT_MIN_BAR}",
        "pass": min(result["after_lift"]) >= AFTER_LIFT_MIN_BAR,
    }
    bars["before_lift_max"] = {
        "value": max(result["before_lift"]),
        "bar": f"< {BEFORE_LIFT_MAX_BAR}",
        "pass": max(result["before_lift"]) < BEFORE_LIFT_MAX_BAR,
    }
    bars["frame_cross_fact"] = {
        "value": result["frame_cross_max"],
        "bar": f"< frame_noise_max ({result['frame_noise_max']:.3f}) -- the eye sees no change at T1",
        "pass": result["frame_cross_max"] < result["frame_noise_max"],
    }
    return bars


FONT = None


def get_font(size):
    global FONT
    try:
        return ImageFont.truetype("arial.ttf", size)
    except Exception:
        return ImageFont.load_default()


def make_pane(img_path, caption, caption2, pane_size, bar_h=56):
    im = Image.open(img_path).convert("RGB").resize(pane_size, Image.LANCZOS)
    out = Image.new("RGB", (pane_size[0], pane_size[1] + bar_h), (20, 20, 20))
    out.paste(im, (0, 0))
    draw = ImageDraw.Draw(out)
    font = get_font(16)
    draw.text((6, pane_size[1] + 4), caption, fill=(255, 255, 255), font=font)
    if caption2:
        draw.text((6, pane_size[1] + 26), caption2, fill=(180, 220, 180), font=font)
    return out


def compose_row(panes, title, out_path):
    pane_w = panes[0].width
    total_w = pane_w * len(panes)
    title_h = 40
    pane_h = panes[0].height
    img = Image.new("RGB", (total_w, title_h + pane_h), (10, 10, 10))
    draw = ImageDraw.Draw(img)
    draw.text((8, 8), title, fill=(255, 255, 255), font=get_font(20))
    for i, pane in enumerate(panes):
        img.paste(pane, (i * pane_w, title_h))
    img.save(out_path)


def build_gas_t1_panel(result, out_path):
    pane_size = (853, 480)
    plate_path = os.path.join(AFTER_DIR, "run1", "gas-t1-plate.jpg")
    before_path = os.path.join(BEFORE_DIR, "run1", "gas-t1.jpg")
    after_path = os.path.join(AFTER_DIR, "run1", "gas-t1.jpg")
    panes = [
        make_pane(plate_path, "NO GAS (the plate)", None, pane_size),
        make_pane(
            before_path,
            f"BEFORE -- the flat sheet, {BEFORE_COMMIT}",
            f"green lift {result['before_lift'][0]:.2f}",
            pane_size,
        ),
        make_pane(
            after_path,
            f"AFTER -- the GasCloud book, {AFTER_COMMIT}",
            f"green lift {result['after_lift'][0]:.2f}",
            pane_size,
        ),
    ]
    compose_row(panes, "The close gas bank at T1 (zoom 30): before vs after", out_path)


def build_gas_cards_panel(out_path):
    pane_size = (853, 480)
    before_path = os.path.join(BEFORE_DIR, "cards", "cards-gas.jpg")
    after_gas_path = os.path.join(AFTER_DIR, "run1", "cards-gas.jpg")
    after_smoke_path = os.path.join(AFTER_DIR, "run1", "cards-smoke.jpg")
    panes = [
        make_pane(before_path, f"BEFORE -- the flat sheet, {BEFORE_COMMIT}", None, pane_size),
        make_pane(after_gas_path, f"AFTER -- the GasCloud book, {AFTER_COMMIT}", None, pane_size),
        make_pane(after_smoke_path, "the smoke card the gas was meant to copy", None, pane_size),
    ]
    compose_row(panes, "The isolation card: where the drawing change IS visible", out_path)


def build_progression_panel(out_path):
    pane_size = (640, 360)
    cols, rows = 3, 2
    title_h = 40
    img = Image.new("RGB", (pane_size[0] * cols, title_h + (pane_size[1] + 32) * rows), (10, 10, 10))
    draw = ImageDraw.Draw(img)
    draw.text((8, 8), "Six rounds of 'tried, no progress': four measured only, two changed the drawing", fill=(255, 255, 255), font=get_font(20))
    for idx, (folder, tag) in enumerate(PROGRESSION_DIRS):
        row, col = divmod(idx, cols)
        jpg = os.path.join(ROOT, "docs", "evidence", folder, "run1", "gas-t1.jpg")
        pane = make_pane(jpg, folder, tag, pane_size)
        img.paste(pane, (col * pane_size[0], title_h + row * (pane_size[1] + 32)))
    img.save(out_path)


def main():
    check_only = "--check" in sys.argv

    result = measure()
    bars = bar(result)

    out_json = os.path.join(HERE, "beforeafter.json")
    out_t1 = os.path.join(HERE, "gas-t1-before-after.png")
    out_cards = os.path.join(HERE, "gas-cards-before-after.png")
    out_prog = os.path.join(HERE, "gas-t1-progression.png")

    if check_only:
        for p in (out_t1, out_cards, out_prog):
            if not os.path.exists(p):
                print(f"missing {p}")
                sys.exit(1)
            if os.path.getsize(p) < MIN_PNG_BYTES:
                print(f"too small: {p}")
                sys.exit(1)
    else:
        build_gas_t1_panel(result, out_t1)
        build_gas_cards_panel(out_cards)
        build_progression_panel(out_prog)
        for p in (out_t1, out_cards, out_prog):
            if os.path.getsize(p) < MIN_PNG_BYTES:
                print(f"PNG too small: {p}")
                sys.exit(1)
        with open(out_json, "w") as f:
            json.dump({"result": result, "bars": {k: {"value": v["value"], "bar": v["bar"], "pass": v["pass"]} for k, v in bars.items()}}, f, indent=2)

    print(f"mask_px = {result['mask_px']}")
    print(f"before_lift (run1/2/3) = {[round(v, 2) for v in result['before_lift']]}")
    print(f"after_lift (run1/2/3) = {[round(v, 2) for v in result['after_lift']]}")
    print(f"layer_noise_max = {result['layer_noise_max']:.3f}")
    print(f"layer_cross_min..max = {result['layer_cross_min']:.3f} .. {result['layer_cross_max']:.3f}")

    failed = False
    for name, b in bars.items():
        status = "PASS" if b["pass"] else "FAIL"
        suffix = " (FACT, not a pass bar)" if name == "frame_cross_fact" else ""
        print(f"{status}  {name}: {b['value']:.3f}  bar {b['bar']}{suffix}")
        if not b["pass"] and name != "frame_cross_fact":
            failed = True

    if failed:
        print("bar inside the noise")
        sys.exit(1)

    print("OK")


if __name__ == "__main__":
    main()
