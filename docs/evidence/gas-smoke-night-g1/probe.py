# Phase: gas-smoke-night-g1 (the owner, 2026-10-20: reshoot gas and smoke, day and night, three times each, from
# one locked camera; each cloud's night contrast outside the spread of its three day captures, and the bar outside
# the spread of three captures of the current build). Contrast definition copied verbatim from
# docs/evidence/gas-smoke-night/probe.py: the cloud box is the centre 40 % x 30 % of the frame; the ground is two
# boxes of the same rows at the left and right tenth. A cloud pixel's contrast is Michelson against the ground
# median, |Lc - Lg| / (Lc + Lg), and a still's number is the median over the cloud box. Luma is Rec.709 on 0-1,
# CaptureRig's own scale (0.2126 R + 0.7152 G + 0.0722 B, bytes / 255) — never mixed with shotstats.py's 0-255.
#
# Measures all 12 stills written by TW.Tests.GasNightStills (TW_STILLS_FIELD=NightMud -> *_nightmud_{0,1,2}.png,
# TW_STILLS_FIELD=Winter -> *_winter_{0,1,2}.png), writes probe.json, prints a line per cloud, and exits 1 on any
# failed check (unlike the old probe, which only printed). Three checks per cloud:
#   1. locked camera: the six sibling JSONs agree on cam_x/cam_y/cam_z/cam_pitch/cam_yaw within 0.01, and every
#      pose_error_m <= 0.01 (the owner's "one locked camera").
#   2. night outside the day spread: min(night three) > max(day three), and the gap is wider than either field's
#      own spread.
#   3. the bar outside the spread: BAR = 0.12 (the phase's, GasSmokeNightTests.Threshold); BAR < min(night three)
#      and min(night three) - BAR > max(night three) - min(night three).
# Needs Pillow.
import json
import os
import statistics
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
CLOUD_W, CLOUD_H = 0.40, 0.30   # the centre box, as a fraction of the frame
GROUND_W = 0.10                 # the left and right tenth, over the cloud box's rows
BAR = 0.12                      # GasSmokeNightTests.Threshold
CLOUDS = ("gas", "smoke")
FIELDS = {"night": "nightmud", "day": "winter"}
REPEATS = 3
POSE_TOL = 0.01
DEFINITION = ("contrast = median over the centre 40 % x 30 % box of |Lc - Lg| / (Lc + Lg), Lg the median luma of the "
              "left and right tenth over the same rows; luma Rec.709 0-1 (CaptureRig's scale); zoom 30 stills")


def luma_rows(path):
    from PIL import Image
    im = Image.open(path).convert("RGB")
    w, h = im.size
    px = im.load()
    return px, w, h


def boxes(w, h):
    cx0, cx1 = int(w * (0.5 - CLOUD_W / 2)), int(w * (0.5 + CLOUD_W / 2))
    cy0, cy1 = int(h * (0.5 - CLOUD_H / 2)), int(h * (0.5 + CLOUD_H / 2))
    gx = int(w * GROUND_W)
    return (cx0, cx1, cy0, cy1), gx


def lumas(px, x0, x1, y0, y1):
    out = []
    for y in range(y0, y1):
        for x in range(x0, x1):
            r, g, b = px[x, y]
            out.append((0.2126 * r + 0.7152 * g + 0.0722 * b) / 255.0)
    return out


def measure(path):
    px, w, h = luma_rows(path)
    (cx0, cx1, cy0, cy1), gx = boxes(w, h)
    cloud = lumas(px, cx0, cx1, cy0, cy1)
    ground = lumas(px, 0, gx, cy0, cy1) + lumas(px, w - gx, w, cy0, cy1)
    lg = statistics.median(ground)
    con = [abs(lc - lg) / (lc + lg) if (lc + lg) > 0 else 0.0 for lc in cloud]
    return statistics.median(con), lg, statistics.median(cloud)


def stem(cloud, field):
    return "%s_%s" % (cloud, FIELDS[field])


def main():
    ok = True
    out = {"definition": DEFINITION, "bar": BAR, "per_still": {}, "per_cloud": {}}
    night_grounds = []
    for cloud in CLOUDS:
        per_field = {}
        poses = []
        for field in FIELDS:
            cons, lgs, lcs, errs = [], [], [], []
            for k in range(REPEATS):
                base = "%s_%d" % (stem(cloud, field), k)
                png = os.path.join(HERE, base + ".png")
                jsn = os.path.join(HERE, base + ".json")
                con, lg, lc = measure(png)
                meta = json.load(open(jsn, encoding="utf-8"))
                out["per_still"][base] = {
                    "contrast_median": round(con, 5), "ground_median": round(lg, 5), "cloud_median": round(lc, 5)}
                cons.append(con); lgs.append(lg); lcs.append(lc); errs.append(meta["pose_error_m"])
                poses.append((meta["cam_x"], meta["cam_y"], meta["cam_z"], meta["cam_pitch"], meta["cam_yaw"]))
            per_field[field] = {"min": min(cons), "median": statistics.median(cons), "max": max(cons), "contrasts": cons}
            if field == "night":
                night_grounds.append(statistics.median(lgs))
            for e in errs:
                if e > POSE_TOL:
                    print("FAIL %s %s: pose_error_m %.4f > %.2f" % (cloud, field, e, POSE_TOL))
                    ok = False
        p0 = poses[0]
        for p in poses[1:]:
            for a, b in zip(p0, p):
                if abs(a - b) > POSE_TOL:
                    print("FAIL %s: camera moved across captures (%.4f vs %.4f)" % (cloud, a, b))
                    ok = False

        night = per_field["night"]; day = per_field["day"]
        day_spread = day["max"] - day["min"]
        night_spread = night["max"] - night["min"]
        gap = night["min"] - day["max"]
        if not (night["min"] > day["max"]):
            print("FAIL %s: night min %.4f does not clear day max %.4f" % (cloud, night["min"], day["max"]))
            ok = False
        elif not (gap > max(day_spread, night_spread)):
            print("FAIL %s: gap %.4f not wider than day spread %.4f / night spread %.4f" %
                  (cloud, gap, day_spread, night_spread))
            ok = False

        if not (BAR < night["min"]):
            print("FAIL %s: bar %.4f not under night min %.4f" % (cloud, BAR, night["min"]))
            ok = False
        elif not (night["min"] - BAR > night_spread):
            print("FAIL %s: night min - bar %.4f not wider than night spread %.4f" %
                  (cloud, night["min"] - BAR, night_spread))
            ok = False

        out["per_cloud"][cloud] = {"day": per_field["day"], "night": per_field["night"]}
        print("%-6s night [%.4f .. %.4f]  day [%.4f .. %.4f]  gap %.4f" %
              (cloud, night["min"], night["max"], day["min"], day["max"], gap))

    out["ground_median_night"] = round(max(night_grounds), 5)   # the brighter of the two clouds' night ground
    with open(os.path.join(HERE, "probe.json"), "w", encoding="utf-8") as f:
        json.dump(out, f, indent=2, sort_keys=True)
        f.write("\n")

    sys.exit(0 if ok else 1)


if __name__ == "__main__":
    main()
