#!/usr/bin/env python3
"""Checks the shelling-flinch-g1 capture set against the owner's complaint and the spread of three live captures.

WHY. The owner's words: "stand-after.png is the shell burst, and the one soldier in stand-before.png is a speck on
the duckboards at zoom 16. The after camera drifts 0.351 m and the pitch falls from 25 to 22.9. There is no walking
man and no frame after the walk cut." FlinchCapture.Set (Assets/_Project/Editor/FlinchCapture.cs) stages a walking
rifleman, shells him, and writes before.png/duck.png/cut.png plus their CaptureRig .json measurements and a
set.json of the sim state at each shot. This reads those files back and checks, in order:

  1. the camera held still and level (CaptureRig.cs's fix): pose_error_m 0, pitch unchanged, across all three shots;
  2. the man fills the frame (CaptureRig's `fit`, not a zoom-derived speck): man_px at or above the floor measured
     from three live captures of this build (runs.json) minus a margin, never a hard-coded guess;
  3. the duck itself (AnimationController.cs:590-595, commit 0e68ac687): before.png's clip is not a flinch, duck.png
     (0.3 s after the burst) and cut.png (0.6 s after, past where the walk cut used to win) both are.

Run from the repo root or this folder: `python docs/evidence/shelling-flinch-g1/probe.py` (no deps beyond stdlib).
"""
import json
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
FLINCH_CLIPS = {"Duck", "KneelFlinch"}
MARGIN = 40.0   # px below the three runs' floor still counts as "fills the frame", not "a speck"


def load(name):
    return json.loads((HERE / name).read_text(encoding="utf-8"))


def fail(msg, failures):
    failures.append(msg)
    print("FAIL:", msg)


def main():
    failures = []

    runs = load("runs.json")
    set_json = load("set.json")
    shots = {s["path"].rsplit(".", 1)[0]: s for s in set_json["shots"]}

    for stem in ("before", "duck", "cut"):
        png = HERE / f"{stem}.png"
        if not png.exists():
            fail(f"{stem}.png is missing", failures)
        measured = load(f"{stem}.json")

        # 1. the camera: locked off, not drifting or tipping (CaptureRig.cs's CameraShake.Strength = 0f fix)
        if abs(measured["pose_error_m"]) > 0.01:
            fail(f"{stem}: pose_error_m {measured['pose_error_m']} (camera drifted off where it was posed)", failures)
        if abs(measured["pitch"] - measured["cam_pitch"]) > 0.5:
            fail(f"{stem}: asked for pitch {measured['pitch']}, camera ended at {measured['cam_pitch']}", failures)

        # 2. fills the frame (CaptureRig.Shot's `fit`, not the tactical zoom)
        floor = runs["man_px_min"] - MARGIN
        if measured["man_px"] < floor:
            fail(f"{stem}: man_px {measured['man_px']:.1f} below the floor {floor:.1f} (three runs: {runs['man_px_min']:.1f}-{runs['man_px_max']:.1f}) — a speck again", failures)

        print(f"{stem}: man_px={measured['man_px']:.1f} pose_error_m={measured['pose_error_m']:.4f} "
              f"pitch={measured['cam_pitch']:.2f} clip={shots.get(stem, {}).get('clip')}")

    # 3. the duck itself: not flinching before the burst, flinching at 0.3 s and still at 0.6 s
    before_clip = shots.get("before", {}).get("clip")
    duck_clip = shots.get("duck", {}).get("clip")
    cut_clip = shots.get("cut", {}).get("clip")
    if before_clip in FLINCH_CLIPS:
        fail(f"before.png: already flinching ({before_clip}) — the burst hasn't happened yet", failures)
    if duck_clip not in FLINCH_CLIPS:
        fail(f"duck.png (0.3 s after the burst): clip is {duck_clip}, not a flinch", failures)
    if cut_clip not in FLINCH_CLIPS:
        fail(f"cut.png (0.6 s after the burst, past the walk cut): clip is {cut_clip}, not a flinch — the frame that didn't exist before", failures)

    if failures:
        print(f"\n{len(failures)} check(s) failed")
        sys.exit(1)
    print("\nall checks passed: camera locked, the man fills the frame, and the duck holds through the walk cut")
    sys.exit(0)


if __name__ == "__main__":
    main()
