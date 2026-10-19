# Measures the sapper's pose shift between idle.png and laying.png. The two stills are the same locked-off camera
# over the same frozen weather, so the one sizeable, contiguous patch of pixel difference is him: a connected-
# components pass over the big differences finds that patch (the largest blob), and everything is cropped to its
# box plus a margin, so the alignment below compares only his own silhouette, not the lamp's halo or acres of
# unrelated trench that would otherwise swamp it. Inside that box, for every column that varies enough in both crops
# (std > 6) the integer vertical shift in -maxShift..maxShift that minimises the SAD between the two columns is
# found; a column is kept only when shifting actually beats not shifting. pose_delta_px is the median of the kept
# columns' |shift|. Every number here is read off the pixels; nothing is typed in.
import json
import os

import numpy as np
from PIL import Image
from scipy import ndimage

HERE = os.path.dirname(os.path.abspath(__file__))


def load_grey(name):
    img = Image.open(os.path.join(HERE, name)).convert("L")
    return np.asarray(img, dtype=np.float64)


def subject_box(idle, laying, threshold=40, margin=20):
    diff = np.abs(idle - laying)
    mask = diff > threshold
    h, w = idle.shape
    labels, n = ndimage.label(mask)
    if n == 0:
        return 0, h, 0, w
    sizes = ndimage.sum(mask, labels, range(1, n + 1))
    biggest = int(np.argmax(sizes)) + 1
    ys, xs = np.where(labels == biggest)
    y0 = max(0, int(ys.min()) - margin)
    y1 = min(h, int(ys.max()) + margin)
    x0 = max(0, int(xs.min()) - margin)
    x1 = min(w, int(xs.max()) + margin)
    return y0, y1, x0, x1


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


def main():
    idle_full = load_grey("idle.png")
    laying_full = load_grey("laying.png")
    h = min(idle_full.shape[0], laying_full.shape[0])
    w = min(idle_full.shape[1], laying_full.shape[1])
    idle_full, laying_full = idle_full[:h, :w], laying_full[:h, :w]

    y0, y1, x0, x1 = subject_box(idle_full, laying_full)
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
    result = {
        "pose_delta_px": pose_delta_px,
        "columns": len(shifts),
        "box": [int(y0), int(y1), int(x0), int(x1)],
        "idle": "idle.png",
        "laying": "laying.png",
    }
    with open(os.path.join(HERE, "probe.json"), "w", encoding="utf-8") as f:
        json.dump(result, f, indent=2)
    print(result)


if __name__ == "__main__":
    main()
