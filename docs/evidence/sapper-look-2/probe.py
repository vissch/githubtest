"""Measures the Sapper's own look against the Engineer's, pixel for pixel: usage `python probe.py` (run from this
folder, or pass the folder as argv[1]). Four stills, same slot/pose/zoom, time held (see note.md / plan.md for how
they were taken): engineer.png, engineer-again.png (the noise floor), sapper.png (same man, same frame, archetype
switched through SimHost.WriteWorlds), ground.png (the man despawned, same frame). Writes probe.json.
"""
import json, os, sys
import numpy as np
from PIL import Image

folder = sys.argv[1] if len(sys.argv) > 1 else os.path.dirname(os.path.abspath(__file__))

def load(name):
    return np.asarray(Image.open(os.path.join(folder, name)).convert("RGB")).astype(np.int16)

engineer = load("engineer.png")
again = load("engineer-again.png")
sapper = load("sapper.png")
ground = load("ground.png")

def diffmask(a, b, thresh=10):
    return (np.abs(a - b).max(axis=-1) > thresh)

man = diffmask(engineer, ground)
kit = diffmask(sapper, engineer)
noise = diffmask(again, engineer)

man_px = int(man.sum())
kit_px = int(kit.sum())
noise_px = int(noise.sum())

silhouette_diff_vs_engineer = (kit_px / man_px) if man_px else 0.0
noise_frac = (noise_px / man_px) if man_px else 0.0

THRESHOLD = 0.08
NOISE_MAX = 0.02

result = {
    "engineer": "engineer.png",
    "engineer_again": "engineer-again.png",
    "sapper": "sapper.png",
    "ground": "ground.png",
    "man_px": man_px,
    "kit_px": kit_px,
    "noise_px": noise_px,
    "silhouette_diff_vs_engineer": silhouette_diff_vs_engineer,
    "noise_frac": noise_frac,
    "threshold": THRESHOLD,
    "noise_max": NOISE_MAX,
}

with open(os.path.join(folder, "probe.json"), "w", encoding="utf-8") as f:
    json.dump(result, f, indent=2)

ok = silhouette_diff_vs_engineer >= THRESHOLD and noise_frac < NOISE_MAX
print("%s diff=%.4f (>= %.2f) noise=%.4f (< %.2f) man_px=%d kit_px=%d" % (
    "OK  " if ok else "FAIL", silhouette_diff_vs_engineer, THRESHOLD, noise_frac, NOISE_MAX, man_px, kit_px))
sys.exit(0 if ok else 1)
