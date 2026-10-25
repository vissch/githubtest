# Measures the sapper's pose shift between idle.png and laying.png, for all three close-framed runs of commit
# a9694b1f0f (run1 offset 0, run2 offset 9, run3 offset 18). Same method as
# docs/evidence/sapper-lay-mine-anim/probe.py (read its header): the largest connected blob of |idle-laying| > 40
# is his silhouette, crop to its box, and for every column that varies enough in both crops (std > 6) take the
# integer vertical shift that beats no shift; pose_delta_px is the median of the kept columns' |shift|.
#
# Differences from the far-pair probe, all because this framing is close and lit by a lamp:
#   - box margin 2, not 20: a wide margin pulls in static background columns and drags the median down;
#   - it gates every run (run1, run2, run3), not one pair;
#   - two guards per run: blob_px >= 2000 (a speckle of noise is not a figure) and the box must contain the frame
#     centre (the shot is aimed at him, so his blob is the middle one -- this is what stops a man dying in frame,
#     fallen: 1 in run1's laying.json, from being measured instead of the sapper);
#   - blown_px_in_box and blown_px_in_head (its top third) count luma >= 250 pixels inside the box: the lamp
#     measure, a white disc on the face is a patch of blown pixels inside his outline;
#   - it also measures the committed far pair ../sapper-lay-mine-anim/{idle,laying}.png as base_delta_px, the noise
#     floor the owner is complaining about (7.0 px over 37 columns, docs/evidence/sapper-lay-mine-anim/probe.json,
#     commit 5d18b98720), and refuses a pose bar at or under it.
#
# Bars (step 5 of the plan), computed from the three measured runs of a9694b1f0f, nothing typed in by taste:
#   pose_delta_px per run: see probe.json after a run; POSE_DELTA_PX_BAR = floor(min - max(spread, 1)), so the bar
#   sits at least one whole pixel below the lowest run, and a whole spread below it when the runs disagree more
#   than that -- outside the spread, not one pixel under a single pair.
#   COLUMNS_BAR by the same rule on the three column counts (the owner's "over 37 columns" is what this must beat).
#   BLOWN_IN_BOX_BAR is a ceiling: max of the three blown_px_in_box + 25, or 25 if all three read 0.
import json
import os

import numpy as np
from PIL import Image
from scipy import ndimage

HERE = os.path.dirname(os.path.abspath(__file__))
FAR_DIR = os.path.join(HERE, "..", "sapper-lay-mine-anim")
RUNS = ["run1", "run2", "run3"]

BLOWN_LUMA = 250
BOX_MARGIN = 2
MIN_BLOB_PX = 2000


def load_grey(path):
    img = Image.open(path).convert("L")
    return np.asarray(img, dtype=np.float64)


def subject_box(idle, laying, threshold=40, margin=BOX_MARGIN):
    diff = np.abs(idle - laying)
    mask = diff > threshold
    h, w = idle.shape
    labels, n = ndimage.label(mask)
    if n == 0:
        return (0, h, 0, w), 0
    sizes = ndimage.sum(mask, labels, range(1, n + 1))
    biggest = int(np.argmax(sizes)) + 1
    blob_px = int(sizes[biggest - 1])
    ys, xs = np.where(labels == biggest)
    y0 = max(0, int(ys.min()) - margin)
    y1 = min(h, int(ys.max()) + margin)
    x0 = max(0, int(xs.min()) - margin)
    x1 = min(w, int(xs.max()) + margin)
    return (y0, y1, x0, x1), blob_px


def best_shift(idle_col, laying_col, max_shift):
    n = idle_col.shape[0]
    no_shift_sad = np.sum(np.abs(idle_col - laying_col))
    best_dy, best_sad = 0, no_shift_sad
    for dy in range(-max_shift, max_shift + 1):
        if dy == 0:
            continue
        if dy > 0:
            a = idle_col[dy:]
            b = laying_col[: n - dy]
        else:
            a = idle_col[: n + dy]
            b = laying_col[-dy:]
        sad = np.sum(np.abs(a - b)) * n / a.shape[0]
        if sad < best_sad:
            best_sad, best_dy = sad, dy
    return best_dy, best_sad, no_shift_sad


def pose_delta(idle_full, laying_full):
    h = min(idle_full.shape[0], laying_full.shape[0])
    w = min(idle_full.shape[1], laying_full.shape[1])
    idle_full, laying_full = idle_full[:h, :w], laying_full[:h, :w]

    (y0, y1, x0, x1), blob_px = subject_box(idle_full, laying_full)
    idle = idle_full[y0:y1, x0:x1]
    laying = laying_full[y0:y1, x0:x1]
    max_shift = max(1, (y1 - y0) // 2)

    shifts = []
    for x in range(idle.shape[1]):
        idle_col = idle[:, x]
        laying_col = laying[:, x]
        if idle_col.std() <= 6 or laying_col.std() <= 6:
            continue
        dy, sad, no_shift_sad = best_shift(idle_col, laying_col, max_shift)
        if sad < no_shift_sad:
            shifts.append(abs(dy))

    pose_delta_px = float(np.median(shifts)) if shifts else 0.0

    box_h, box_w = idle_full[y0:y1, x0:x1].shape
    centre_in_box = (y0 <= h // 2 <= y1) and (x0 <= w // 2 <= x1)

    blown = laying_full[y0:y1, x0:x1] >= BLOWN_LUMA
    blown_px_in_box = int(np.sum(blown))
    head_rows = max(1, box_h // 3)
    blown_px_in_head = int(np.sum(blown[:head_rows, :]))

    return {
        "pose_delta_px": pose_delta_px,
        "columns": len(shifts),
        "box": [int(y0), int(y1), int(x0), int(x1)],
        "blob_px": blob_px,
        "centre_in_box": bool(centre_in_box),
        "blown_px_in_box": blown_px_in_box,
        "blown_px_in_head": blown_px_in_head,
    }


def main():
    results = {}
    failures = []

    for run in RUNS:
        d = os.path.join(HERE, run)
        idle = load_grey(os.path.join(d, "idle.png"))
        laying = load_grey(os.path.join(d, "laying.png"))
        r = pose_delta(idle, laying)
        results[run] = r
        if r["blob_px"] < MIN_BLOB_PX:
            failures.append("%s: blob_px %d < %d (noise, not a figure)" % (run, r["blob_px"], MIN_BLOB_PX))
        if not r["centre_in_box"]:
            failures.append("%s: box does not contain the frame centre (wrong blob measured)" % run)

    far_idle = load_grey(os.path.join(FAR_DIR, "idle.png"))
    far_laying = load_grey(os.path.join(FAR_DIR, "laying.png"))
    far = pose_delta(far_idle, far_laying)
    base_delta_px = far["pose_delta_px"]
    base_columns = far["columns"]

    deltas = [results[r]["pose_delta_px"] for r in RUNS]
    columns = [results[r]["columns"] for r in RUNS]
    blown_in_box = [results[r]["blown_px_in_box"] for r in RUNS]

    delta_spread = max(deltas) - min(deltas)
    pose_delta_px_bar = float(np.floor(min(deltas) - max(delta_spread, 1)))

    columns_spread = max(columns) - min(columns)
    columns_bar = int(np.floor(min(columns) - max(columns_spread, 1)))

    blown_in_box_bar = (max(blown_in_box) + 25) if max(blown_in_box) > 0 else 25

    if pose_delta_px_bar <= base_delta_px:
        failures.append(
            "pose_delta_px_bar %.1f at or under base_delta_px %.1f: the close framing bought no pose resolution"
            % (pose_delta_px_bar, base_delta_px)
        )

    bars = {
        "POSE_DELTA_PX_BAR": {
            "value": pose_delta_px_bar,
            "why": "floor(min(%s) - max(spread %.1f, 1)) over the three runs of a9694b1f0f, outside their spread "
            "and clear of the far pair's base_delta_px %.1f" % (deltas, delta_spread, base_delta_px),
        },
        "COLUMNS_BAR": {
            "value": columns_bar,
            "why": "floor(min(%s) - max(spread %d, 1)) over the three runs of a9694b1f0f, beating the far pair's "
            "%d columns" % (columns, columns_spread, base_columns),
        },
        "BLOWN_IN_BOX_BAR": {
            "value": blown_in_box_bar,
            "why": "max(%s) + 25, a ceiling on blown face pixels so the lamp stays out of his silhouette"
            % blown_in_box,
        },
    }

    for run in RUNS:
        r = results[run]
        if r["pose_delta_px"] <= bars["POSE_DELTA_PX_BAR"]["value"]:
            failures.append(
                "%s: pose_delta_px %.1f <= POSE_DELTA_PX_BAR %.1f"
                % (run, r["pose_delta_px"], bars["POSE_DELTA_PX_BAR"]["value"])
            )
        if r["columns"] <= bars["COLUMNS_BAR"]["value"]:
            failures.append("%s: columns %d <= COLUMNS_BAR %d" % (run, r["columns"], bars["COLUMNS_BAR"]["value"]))
        if r["blown_px_in_box"] > bars["BLOWN_IN_BOX_BAR"]["value"]:
            failures.append(
                "%s: blown_px_in_box %d > BLOWN_IN_BOX_BAR %d"
                % (run, r["blown_px_in_box"], bars["BLOWN_IN_BOX_BAR"]["value"])
            )

    out = {
        "runs": results,
        "far_pair": {"pose_delta_px": base_delta_px, "columns": base_columns},
        "base_delta_px": base_delta_px,
        "spread": {"pose_delta_px": delta_spread, "columns": columns_spread},
        "bars": bars,
    }
    with open(os.path.join(HERE, "probe.json"), "w", encoding="utf-8") as f:
        json.dump(out, f, indent=2)
    print(json.dumps(out, indent=2))

    if failures:
        for line in failures:
            print("FAIL:", line)
        raise SystemExit(1)


if __name__ == "__main__":
    main()
