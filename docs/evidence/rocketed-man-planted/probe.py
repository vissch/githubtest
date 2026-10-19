#!/usr/bin/env python
"""Does the planted man read at the standard zoom? Measured from the stills' own pixels.

The sidecar's own `contrast_median` cannot answer this: CaptureRig measures a disc on the CHEST of every LIVING man
(VATRenderer draws the fallen elsewhere), so a frame whose only subject is a pair of legs standing in the mud is
scored off the riflemen in the background. This reads the stills instead, at the place the legs are drawn.

Where that place is, from the sidecar alone:
    dist      = how far the camera is from the point it aims at: the horizontal run over cos(pitch);
    tan(fov/2) = zoom * tan 30 / dist                          (CaptureRig.Pose, CaptureRig.cs:167);
    pxPerM    = h / (2 * dist * tan(fov/2))                    pixels per metre on the focus plane;
    vpm       = pxPerM * cos(cam_pitch)                        pixels per metre of WORLD HEIGHT.
Each still is aimed at ground level under the man (aimY), so the frame centre is his feet-in-the-mud, and his legs
stand DeathGags.PlantLegsAboveM = 0.905 m above it: `legs` pixels.

    man = median Rec.709 luma (0-1) of the disc r = max(2, 0.18 * legs) at 0.45 * vpm above centre (mid-shin);
    bg  = median of the ring 1.3 .. 2.3 * legs around centre (the mud and kit behind him);
    contrast = |man - bg| / (bg + 0.02)        -- the same ratio CaptureRig uses, so the numbers compare.

Writes probe.json beside the stills: contrast_median over them, and every sample, so the number can be checked.
"""
import json, math, os, statistics, sys

from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
LEGS_M = 0.905          # DeathGags.PlantLegsAboveM: the owner's pick A, waist deep
SHIN_M = 0.45           # where on the legs the disc sits, in metres above the mud


def luma(px):
    r, g, b = px[0] / 255.0, px[1] / 255.0, px[2] / 255.0
    return 0.2126 * r + 0.7152 * g + 0.0722 * b


def disc(lum, w, h, cx, cy, r_in, r_out):
    """Median luma of the pixels between two radii of (cx, cy); r_in 0 makes it a disc."""
    vals = []
    for y in range(max(0, cy - r_out), min(h, cy + r_out + 1)):
        for x in range(max(0, cx - r_out), min(w, cx + r_out + 1)):
            d = math.hypot(x - cx, y - cy)
            if r_in <= d <= r_out:
                vals.append(lum[y * w + x])
    return statistics.median(vals) if vals else None


def probe(png):
    side = os.path.splitext(png)[0] + ".json"
    s = json.load(open(side, encoding="utf-8"))
    img = Image.open(png).convert("RGB")
    w, h = img.size
    lum = [luma(p) for p in img.getdata()]

    run = math.hypot(s["cam_x"] - s["focus_x"], s["cam_z"] - s["focus_z"])
    pitch = math.radians(s["cam_pitch"])
    dist = run / math.cos(pitch)
    tan_half = s["zoom"] * math.tan(math.radians(30.0)) / dist
    px_per_m = h / (2.0 * dist * tan_half)
    vpm = px_per_m * math.cos(pitch)

    legs = LEGS_M * vpm
    cx, cy = w // 2, h // 2                       # image row 0 is the TOP, so "above centre" is a smaller y
    shin_y = int(round(cy - SHIN_M * vpm))
    r_man = max(2, int(round(0.18 * legs)))
    man = disc(lum, w, h, cx, shin_y, 0, r_man)
    bg = disc(lum, w, h, cx, cy, int(round(1.3 * legs)), int(round(2.3 * legs)))
    contrast = abs(man - bg) / (bg + 0.02)
    return {
        "still": os.path.basename(png), "legs_px": round(legs, 1), "px_per_m_vertical": round(vpm, 2),
        "man_luma": round(man, 5), "bg_luma": round(bg, 5), "contrast": round(contrast, 5),
        "man_disc_r_px": r_man, "cam_dist_m": round(dist, 2), "fov_deg": round(2 * math.degrees(math.atan(tan_half)), 2),
    }


def main():
    stills = sorted(f for f in os.listdir(HERE) if f.startswith("plant_") and f.endswith(".png"))
    if not stills:
        print("no plant_*.png beside this script", file=sys.stderr)
        return 1
    samples = [probe(os.path.join(HERE, f)) for f in stills]
    med = statistics.median(s["contrast"] for s in samples)
    out = {"contrast_median": round(med, 5), "samples": samples, "stills": stills}
    with open(os.path.join(HERE, "probe.json"), "w", encoding="utf-8") as fh:
        json.dump(out, fh, indent=2)
        fh.write("\n")
    for s in samples:
        print("%s legs %5.1f px  man %.4f  bg %.4f  contrast %.4f" % (s["still"], s["legs_px"], s["man_luma"], s["bg_luma"], s["contrast"]))
    print("contrast_median %.4f over %d stills" % (med, len(samples)))
    return 0


if __name__ == "__main__":
    sys.exit(main())
