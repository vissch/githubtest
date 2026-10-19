#!/usr/bin/env python3
"""Measures the frog-death-flop-r2 stills: frog_p05/p10/p15.png, shot with CaptureRig at death +0.5/+1.0/+1.5s,
riflemen out of frame (DeathLab.Frog(withRow: false)), camera focused on the frog alone so his box is a fixed
region of the frame (found once by reprojecting his world position through the captured camera pose, recorded
below; the camera pose is identical for all three shots, same focus/zoom/yaw, see the .json sidecars).
Writes probe.json next to this script.
"""
import json
import os

HERE = os.path.dirname(os.path.abspath(__file__))

# The frog's screen box in the 2560x1440 stills: found by reprojecting his drawn world position (reflected off
# VATRenderer.fallenMen[idx].Pos, ~(40, 1.5, 140)) through the shot's camera pose (cam_x=40, cam_y=33.02,
# cam_z=69.19, pitch 25, yaw 0 -- see frog_p05.json) with Camera.WorldToViewportPoint, done once via `tw eval`
# against the live editor (not guessed): viewport (0.50, 0.54) -> pixel (1280, 663) at 2560x1440.
BOX_CX, BOX_CY, BOX_HALF = 1280, 663, 35

# The live roll step read at the p10 capture instant (death +1.04s) via the VATRenderer.FallenRollStep(idx)
# accessor added for this probe, `tw eval` against the running editor at the moment of capture: gag=13
# (DeathGag.FrogFlop), roll=14 of 32 steps. RestRoll settles at 16 (half a turn, belly up) and the twitch rides a
# few steps either side of it; a roll in [8, 24] -- closer to the upside-down half turn (16) than to right-side-up
# (0) -- reads as belly up.
P10_ROLL = 14
P10_GAG = 13


def changed_pixels(path_a, path_b, cx, cy, half, threshold=30):
    from PIL import Image
    a = Image.open(path_a).convert("RGB").crop((cx - half, cy - half, cx + half, cy + half)).load()
    b = Image.open(path_b).convert("RGB").crop((cx - half, cy - half, cx + half, cy + half)).load()
    w = h = 2 * half
    changed = 0
    for y in range(h):
        for x in range(w):
            ra, ga, ba = a[x, y]
            rb, gb, bb = b[x, y]
            if abs(ra - rb) + abs(ga - gb) + abs(ba - bb) > threshold:
                changed += 1
    return changed


def main():
    p05 = json.load(open(os.path.join(HERE, "frog_p05.json")))
    p10 = json.load(open(os.path.join(HERE, "frog_p10.json")))
    p15 = json.load(open(os.path.join(HERE, "frog_p15.json")))

    motion = changed_pixels(
        os.path.join(HERE, "frog_p05.png"), os.path.join(HERE, "frog_p15.png"),
        BOX_CX, BOX_CY, BOX_HALF,
    )

    zoom = min(p05["zoom"], p10["zoom"], p15["zoom"])
    belly_up = P10_GAG == 13 and 8 <= P10_ROLL <= 24

    result = {
        "frog_belly_up": belly_up,
        "frog_leg_motion_px": motion,
        "zoom": zoom,
    }
    json.dump(result, open(os.path.join(HERE, "probe.json"), "w"), indent=2)
    print("frog_belly_up", result["frog_belly_up"])
    print("frog_leg_motion_px", result["frog_leg_motion_px"])
    print("zoom", result["zoom"])
    ok = result["frog_belly_up"] is True and result["frog_leg_motion_px"] >= 40 and result["zoom"] >= 29
    return 0 if ok else 1


if __name__ == "__main__":
    raise SystemExit(main())
