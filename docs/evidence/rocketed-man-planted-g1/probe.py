#!/usr/bin/env python
"""Does the kicking planted man read at the standard zoom (T1, zoom 30)? Measured from the stills' own
pixels, same geometry and luma/contrast ratio as docs/evidence/rocketed-man-planted/probe.py so the numbers
compare, but this one measures shin AND boot separately (that unit's single "chest" guess is what let
plant_2 pass through with a frame centred on bare mud) and sets its bar from three `base_*.png` captures of
the CURRENT build taken in this unit, not from an assumed target.

Where shin and boot are, from the sidecar alone (unchanged from the old probe):
    dist      = how far the camera is from the point it aims at: the horizontal run over cos(pitch);
    tan(fov/2) = zoom * tan 30 / dist                          (CaptureRig.Pose, CaptureRig.cs:167);
    pxPerM    = h / (2 * dist * tan(fov/2))                    pixels per metre on the focus plane;
    vpm       = pxPerM * cos(cam_pitch)                        pixels per metre of WORLD HEIGHT.
Each still is aimed at ground level under the man (aimY), so the frame centre is his feet-in-the-mud, and his
legs stand DeathGags.PlantLegsAboveM = 0.905 m above it ("legs" pixels): SHIN_M = 0.45 m up is mid-shin,
BOOT_M = 0.86 m up is the top of the column, just under the boot soles.

    shin/boot = median Rec.709 luma (0-1) of the disc r = max(2, 0.18 * legs) at that height above centre;
    bg        = median of the ring 1.3 .. 2.3 * legs around centre (the mud and kit around him);
    contrast  = |v - bg| / (bg + 0.02)        -- the same ratio CaptureRig uses, so the numbers compare.

The legs-are-really-there guard (what plant_2 in the old unit failed without anyone noticing, since that
probe never looked at the legs themselves): a 1-px-wide column from the centre up to boot height must be
darker than bg on at least 70% of its rows, and legs_px must fall in [20, 30] (the standard zoom's own
geometry, not a framing choice).

The bar is not asserted, it is measured: BAR_SHIN / BAR_BOOT are each the baseline's worst still plus one
full spread width above it (max(base) + (max(base) - min(base))), from the three `base_*.png` — the current
build, same zoom/yaw/pitch, un-curated staging. `--baseline` reports just that measurement.

Writes probe.json beside the stills, and a 3x nearest-neighbour 120x120 crop of each plant_*.png about its
centre (plant_N_zoom.png), so the owner can see the 21 px of leg without reading pixels himself.
"""
import json, math, os, statistics, sys

from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
LEGS_M = 0.905          # DeathGags.PlantLegsAboveM: the owner's pick A, waist deep
SHIN_M = 0.45           # mid-shin, metres above the mud
BOOT_M = 0.86           # just under the boot soles, the top of the standing column
COLUMN_DARK_SHARE = 0.70
LEGS_PX_MIN, LEGS_PX_MAX = 20.0, 30.0


def luma(px):
    r, g, b = px[0] / 255.0, px[1] / 255.0, px[2] / 255.0
    return 0.2126 * r + 0.7152 * g + 0.0722 * b


def disc(lum, w, h, cx, cy, r_in, r_out):
    vals = []
    for y in range(max(0, cy - r_out), min(h, cy + r_out + 1)):
        for x in range(max(0, cx - r_out), min(w, cx + r_out + 1)):
            d = math.hypot(x - cx, y - cy)
            if r_in <= d <= r_out:
                vals.append(lum[y * w + x])
    return statistics.median(vals) if vals else None


def geometry(s, h):
    run = math.hypot(s["cam_x"] - s["focus_x"], s["cam_z"] - s["focus_z"])
    pitch = math.radians(s["cam_pitch"])
    dist = run / math.cos(pitch)
    tan_half = s["zoom"] * math.tan(math.radians(30.0)) / dist
    px_per_m = h / (2.0 * dist * tan_half)
    return px_per_m * math.cos(pitch)       # vpm


def column_guard(lum, w, h, cx, cy, top_y, bg):
    if top_y >= cy:
        return False, 0.0
    rows = range(top_y, cy)
    dark = sum(1 for y in rows if lum[y * w + cx] < bg)
    share = dark / len(rows) if rows else 0.0
    return share >= COLUMN_DARK_SHARE, share


def probe(png):
    side = os.path.splitext(png)[0] + ".json"
    s = json.load(open(side, encoding="utf-8"))
    img = Image.open(png).convert("RGB")
    w, h = img.size
    lum = [luma(p) for p in img.getdata()]

    vpm = geometry(s, h)
    legs = LEGS_M * vpm
    cx, cy = w // 2, h // 2                       # image row 0 is the TOP, so "above centre" is a smaller y
    shin_y = int(round(cy - SHIN_M * vpm))
    boot_y = int(round(cy - BOOT_M * vpm))
    r_man = max(2, int(round(0.18 * legs)))

    shin = disc(lum, w, h, cx, shin_y, 0, r_man)
    boot = disc(lum, w, h, cx, boot_y, 0, r_man)
    bg = disc(lum, w, h, cx, cy, int(round(1.3 * legs)), int(round(2.3 * legs)))
    contrast_shin = abs(shin - bg) / (bg + 0.02)
    contrast_boot = abs(boot - bg) / (bg + 0.02)

    guard_ok, dark_share = column_guard(lum, w, h, cx, cy, boot_y, bg)
    legs_ok = LEGS_PX_MIN <= legs <= LEGS_PX_MAX

    return {
        "still": os.path.basename(png), "legs_px": round(legs, 1), "px_per_m_vertical": round(vpm, 2),
        "shin_luma": round(shin, 5), "boot_luma": round(boot, 5), "bg_luma": round(bg, 5),
        "contrast_shin": round(contrast_shin, 5), "contrast_boot": round(contrast_boot, 5),
        "man_disc_r_px": r_man, "cam_dist_m": round(math.hypot(s["cam_x"] - s["focus_x"], s["cam_z"] - s["focus_z"]) / math.cos(math.radians(s["cam_pitch"])), 2),
        "column_dark_share": round(dark_share, 3), "column_guard": guard_ok, "legs_px_guard": legs_ok,
        "guard": guard_ok and legs_ok,
    }


def zoom_crop(png, out):
    img = Image.open(png).convert("RGB")
    w, h = img.size
    cx, cy = w // 2, h // 2
    crop = img.crop((cx - 20, cy - 20, cx + 20, cy + 20)).resize((120, 120), Image.NEAREST)
    crop.save(out)


def bars(base_samples):
    shin = [s["contrast_shin"] for s in base_samples]
    boot = [s["contrast_boot"] for s in base_samples]
    bar_shin = max(shin) + (max(shin) - min(shin))
    bar_boot = max(boot) + (max(boot) - min(boot))
    return bar_shin, bar_boot, shin, boot


def main():
    argv = sys.argv[1:]
    baseline_only = "--baseline" in argv

    base_files = sorted(f for f in os.listdir(HERE) if f.startswith("base_") and f.endswith(".png"))
    if not base_files:
        print("no base_*.png beside this script", file=sys.stderr)
        return 1
    base_samples = [probe(os.path.join(HERE, f)) for f in base_files]
    bar_shin, bar_boot, base_shin, base_boot = bars(base_samples)

    for s in base_samples:
        print("%-12s legs %5.1f px  shin %.4f  boot %.4f  bg %.4f" % (s["still"], s["legs_px"], s["contrast_shin"], s["contrast_boot"], s["bg_luma"]))
    print("baseline shin %s -> bar_shin %.4f" % (["%.4f" % v for v in base_shin], bar_shin))
    print("baseline boot %s -> bar_boot %.4f" % (["%.4f" % v for v in base_boot], bar_boot))

    if baseline_only:
        clear = bar_shin > max(base_shin) and bar_boot > max(base_boot)
        print("bars sit outside the baseline spread" if clear else "bars DO NOT clear the baseline spread")
        return 0 if clear else 1

    plant_files = sorted(f for f in os.listdir(HERE) if f.startswith("plant_") and f.endswith(".png") and "_zoom" not in f)
    if not plant_files:
        print("no plant_*.png beside this script", file=sys.stderr)
        return 1
    plant_samples = [probe(os.path.join(HERE, f)) for f in plant_files]
    for s in plant_samples:
        print("%-12s legs %5.1f px  shin %.4f  boot %.4f  bg %.4f  guard %s (column %.0f%%)" %
              (s["still"], s["legs_px"], s["contrast_shin"], s["contrast_boot"], s["bg_luma"], s["guard"], s["column_dark_share"] * 100))
        zoom_crop(os.path.join(HERE, s["still"]), os.path.join(HERE, os.path.splitext(s["still"])[0] + "_zoom.png"))

    med_shin = statistics.median(s["contrast_shin"] for s in plant_samples)
    med_boot = statistics.median(s["contrast_boot"] for s in plant_samples)
    all_guard = all(s["guard"] for s in plant_samples)
    no_base_over = max(base_shin) < bar_shin and max(base_boot) < bar_boot
    passed = all_guard and med_shin > bar_shin and med_boot > bar_boot and no_base_over

    out = {
        "bar_shin": round(bar_shin, 5), "bar_boot": round(bar_boot, 5),
        "median_contrast_shin": round(med_shin, 5), "median_contrast_boot": round(med_boot, 5),
        "base_samples": base_samples, "plant_samples": plant_samples, "passed": passed,
    }
    with open(os.path.join(HERE, "probe.json"), "w", encoding="utf-8") as fh:
        json.dump(out, fh, indent=2)
        fh.write("\n")

    print("median shin %.4f %s bar %.4f" % (med_shin, ">" if med_shin > bar_shin else "<=", bar_shin))
    print("median boot %.4f %s bar %.4f" % (med_boot, ">" if med_boot > bar_boot else "<=", bar_boot))
    print("every plant_* guard: %s" % all_guard)
    print("no base_* over either bar: %s" % no_base_over)
    print("PASS" if passed else "FAIL")
    return 0 if passed else 1


if __name__ == "__main__":
    sys.exit(main())
