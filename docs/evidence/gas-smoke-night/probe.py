# Phase: gas-smoke-night (the owner, 2026-10-19). How readable an ability cloud is against the ground it stands on,
# measured off the pixels of the stills beside this file (written by TW.Tests.GasNightStills at zoom 30).
#
# Per still: the cloud box is the centre 40 % x 30 % of the frame; the ground is two boxes of the same rows at the
# left and right tenth. A cloud pixel's contrast is Michelson against the ground median, |Lc - Lg| / (Lc + Lg), and
# the still's number is the median over the cloud box. Luma is Rec.709 on 0-1, CaptureRig's own scale
# (0.2126 R + 0.7152 G + 0.0722 B, bytes / 255) — never mixed with shotstats.py's 0-255.
#
# A field's number is the MIN of its gas and smoke still, so neither cloud can hide behind the other.
# Writes probe.json beside the stills and prints both numbers. Needs Pillow.
import json
import os
import statistics

HERE = os.path.dirname(os.path.abspath(__file__))
CLOUD_W, CLOUD_H = 0.40, 0.30   # the centre box, as a fraction of the frame
GROUND_W = 0.10                 # the left and right tenth, over the cloud box's rows
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


def main():
    out = {"definition": DEFINITION, "per_still": {}}
    for field in ("night", "day"):
        mins = []
        for cloud in ("gas", "smoke"):
            path = os.path.join(HERE, "%s_%s.png" % (cloud, field))
            con, lg, lc = measure(path)
            out["per_still"]["%s_%s" % (cloud, field)] = {
                "contrast_median": round(con, 5), "ground_median": round(lg, 5), "cloud_median": round(lc, 5)}
            mins.append(con)
            if cloud == "gas":
                out["ground_median_" + field] = round(lg, 5)
        out["contrast_median_" + field] = round(min(mins), 5)
    with open(os.path.join(HERE, "probe.json"), "w", encoding="utf-8") as f:
        json.dump(out, f, indent=2, sort_keys=True)
        f.write("\n")
    print("contrast_median_night %.4f" % out["contrast_median_night"])
    print("contrast_median_day   %.4f" % out["contrast_median_day"])
    for k, v in sorted(out["per_still"].items()):
        print("  %-12s contrast %.4f  cloud %.4f  ground %.4f" % (k, v["contrast_median"], v["cloud_median"], v["ground_median"]))


if __name__ == "__main__":
    main()
