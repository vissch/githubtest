"""Measures the Sapper's kit silhouette from the committed CaptureRig stills (GreyboxCorridor night, clock
pinned, rain 0, same spawned slot at the same world position and camera pose; engineer-N.png and sapper-N.png
differ only by that slot's archetype, ground.png by the slot being despawned). Writes probe.json next to this
file. Numbers are read from pixels, never typed. Part 1 of the plan: the portrait keys are added by part 2 and
are left out of probe.json here.

CROP, and why: despawning or re-posing the man while CaptureRig.Hold() has the sim ticks frozen never reaches
the renderer (it only rebuilds a unit's draw entry on a real simulation tick), so ground.png had to be taken a
moment after a brief Release()/Hold() cycle. That cycle resumes Time.timeScale for one real tick and the
night level's lit fires and water catch up in a single jump, moving tens of thousands of pixels that have
nothing to do with the man. Three repeat engineer stills taken back to back under one unbroken Hold (no
Release) measure ZERO noise pixels anywhere in the full 1600x900 frame (see NOISE_FULL_FRAME below) - the
jump is wholly a property of the Release/Hold cycle, not of anything this change touched. CROP_BOX is a
generous box around the man's own on-screen footprint (found from the kit diff itself, which is unaffected by
the despawn cycle) with a wide margin; restricting every comparison to it keeps the measurement about him, not
about a fire two hundred metres off that the despawn cycle happened to relight.
"""
import json
import os

from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
CROP_BOX = (180, 390, 250, 460)  # x0, y0, x1, y1 - around the man's footprint at (50, 50), T1 STANDARD


def load(name):
    return Image.open(os.path.join(HERE, name)).convert("RGB")


def crop(img):
    return img.crop(CROP_BOX)


def diff_px(a, b, threshold=10):
    pa = a.load()
    pb = b.load()
    w, h = a.size
    hits = 0
    for y in range(h):
        for x in range(w):
            ra, ga, ba = pa[x, y]
            rb, gb, bb = pb[x, y]
            d = max(abs(ra - rb), abs(ga - gb), abs(ba - bb))
            if d > threshold:
                hits += 1
    return hits


def main():
    ground = crop(load("ground.png"))
    engineers = [crop(load(f"engineer-{i}.png")) for i in (1, 2, 3)]
    sappers = [crop(load(f"sapper-{i}.png")) for i in (1, 2, 3)]

    noise_full_frame = diff_px(load("engineer-1.png"), load("engineer-2.png"))

    man_px = diff_px(engineers[0], ground)

    kit_ratios = [diff_px(s, engineers[0]) / man_px for s in sappers]
    silhouette_diff_vs_engineer = min(kit_ratios)

    base_ratios = []
    for i in range(3):
        for j in range(i + 1, 3):
            base_ratios.append(diff_px(engineers[i], engineers[j]) / man_px)
    silhouette_diff_vs_engineer_base_max = max(base_ratios)

    probe = {
        "n_captures": 3,
        "man_px": man_px,
        "noise_full_frame_px": noise_full_frame,
        "silhouette_diff_vs_engineer": silhouette_diff_vs_engineer,
        "silhouette_diff_vs_engineer_base_max": silhouette_diff_vs_engineer_base_max,
    }
    with open(os.path.join(HERE, "probe.json"), "w", encoding="utf-8") as f:
        json.dump(probe, f, indent=2)
    print(json.dumps(probe, indent=2))

    bar = 0.15
    base = probe["silhouette_diff_vs_engineer_base_max"]
    val = probe["silhouette_diff_vs_engineer"]
    if not (base < bar <= val):
        print(f"FAIL silhouette_diff_vs_engineer: base_max={base} bar={bar} value={val}")
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
