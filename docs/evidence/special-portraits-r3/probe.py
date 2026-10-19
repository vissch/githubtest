"""The probe for special-portraits-r3: do the Flamer's and the Sentry's portraits really SHOW their kit?

r2's probe proved only that the two pictures are not copies of anything else. The harsh critic still scored them
46/100, because "not a copy" is not "you can see his plate": the Sentry stood in the open with a stick, the Flamer's
wand could not be found in an empty hand, and Flamer-man.json reported contrast_median 0.064 - "men barely separate
from the ground". So this keeps every number r2 measured and adds three that answer the critic directly:

  flamer_man_contrast  the contrast_median CaptureRig itself measured on the committed Flamer-man still (chest disc
                       against a ring of the ground behind him). The owner's bar is 0.15; r2 scored 0.064.
  flamer_wand_px       how many of the 65 536 portrait pixels change when the wand is taken off the same man in the
                       same pose - that is the wand's own area in the card picture. Bar 300.
  sentry_plate_px      the same for the plate. Bar 2000; it is a bigger thing than a wand.

The two no-prop twins are cut by portrait_cut.py through the SAME crop box, mask and resize as the portrait, from a
still shot at the same world moment, so a pixel differs only where the prop was.

usage (from the repo root): python docs/evidence/special-portraits-r3/probe.py
Writes docs/evidence/special-portraits-r3/probe.json and exits 0 only on every bar in this unit's goal. Every number
is measured off the rendered stills and the cut portraits; none is typed.
"""
import glob
import hashlib
import json
import os
import pathlib

from PIL import Image

HERE = pathlib.Path(__file__).resolve().parent
ROOT = HERE.parents[2]
PORTRAITS = ROOT / "trench-warfare-3d" / "Assets" / "_Project" / "UI" / "Skin" / "Portraits"
PLACEHOLDERS = ROOT / "trench-warfare-3d" / "Assets" / "_Project" / "UI" / "Skin" / "placeholders.json"
MINE = ("Flamer", "Sentry")
# portrait -> (the twin cut with the prop hidden, the key its pixel count is reported under, the owner's bar)
PROPS = {
    "Flamer": ("Flamer-nowand.png", "flamer_wand_px", 300),
    "Sentry": ("Sentry-noplate.png", "sentry_plate_px", 2000),
}
SIZE = 256
BAR = 0.04
CONTRAST_BAR = 0.15
CHANNEL = 8          # a pixel counts as changed when any RGBA channel moves by more than this, out of 255


def load(path):
    return Image.open(path).convert("RGBA").resize((SIZE, SIZE), Image.LANCZOS)


def mean_abs_diff(a, b):
    pa, pb = a.getdata(), b.getdata()
    total = 0
    for x, y in zip(pa, pb):
        total += abs(x[0] - y[0]) + abs(x[1] - y[1]) + abs(x[2] - y[2]) + abs(x[3] - y[3])
    return total / float(SIZE * SIZE * 4 * 255)


def changed_px(a, b):
    """Pixels where any RGBA channel moves by more than CHANNEL: the prop's own area in the card picture."""
    n = 0
    for x, y in zip(a.getdata(), b.getdata()):
        if (abs(x[0] - y[0]) > CHANNEL or abs(x[1] - y[1]) > CHANNEL
                or abs(x[2] - y[2]) > CHANNEL or abs(x[3] - y[3]) > CHANNEL):
            n += 1
    return n


def opaque_frac(path):
    im = Image.open(path).convert("RGBA")
    n = im.width * im.height
    return sum(1 for p in im.getdata() if p[3] > 32) / float(n)


def sha1(path):
    return hashlib.sha1(pathlib.Path(path).read_bytes()).hexdigest()


def sidecars():
    """The CaptureRig sidecars committed beside this script, by stem."""
    out = {}
    for side in sorted(glob.glob(str(HERE / "*.json"))):
        if os.path.basename(side).startswith("probe"):
            continue
        try:
            d = json.loads(pathlib.Path(side).read_text(encoding="utf-8"))
        except ValueError:
            continue
        out[os.path.basename(side)[:-5]] = d
    return out


def main():
    files = sorted(glob.glob(str(PORTRAITS / "*.png")))
    imgs = {pathlib.Path(f).stem: load(f) for f in files}
    hashes = {pathlib.Path(f).stem: sha1(f) for f in files}
    placed = []
    if PLACEHOLDERS.exists():
        placed = [e.get("file") for e in json.loads(PLACEHOLDERS.read_text(encoding="utf-8")).get("files", [])]

    cars = sidecars()
    report = {
        "portraits": len(files),
        "bar_min_diff": BAR,
        "bar_man_contrast": CONTRAST_BAR,
        "still_pose_error_m": {k: float(v["pose_error_m"]) for k, v in cars.items() if "pose_error_m" in v},
        "listed_as_placeholder": {n: ("Portraits/%s.png" % n) in placed for n in MINE},
    }
    # the critic's own complaint, read back off the still the portrait was cut from
    report["flamer_man_contrast"] = round(float(cars.get("Flamer-man", {}).get("contrast_median", -1.0)), 4)
    report["sentry_man_contrast"] = round(float(cars.get("Sentry-man", {}).get("contrast_median", -1.0)), 4)

    for name in MINE:
        if name not in imgs:
            report[name.lower() + "_min_diff"] = 0.0
            continue
        worst, against = 1.0, None
        for other, im in imgs.items():
            if other == name:
                continue
            d = mean_abs_diff(imgs[name], im)
            if d < worst:
                worst, against = d, other
        report[name.lower() + "_min_diff"] = round(worst, 6)
        report[name.lower() + "_nearest"] = against
        report[name.lower() + "_opaque_frac"] = round(opaque_frac(PORTRAITS / (name + ".png")), 4)
        report[name.lower() + "_sha1"] = hashes[name]

        twin, key, _bar = PROPS[name]
        path = HERE / twin
        report[key] = changed_px(imgs[name], load(path)) if path.exists() else 0

    mine_hashes = [hashes.get(n) for n in MINE]
    report["unique_hashes"] = all(
        h is not None and sum(1 for o, oh in hashes.items() if oh == h) == 1 for h in mine_hashes
    )

    (HERE / "probe.json").write_text(json.dumps(report, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    print(json.dumps(report, indent=2, sort_keys=True))

    bars = [
        ("flamer_min_diff", report.get("flamer_min_diff", 0), BAR),
        ("sentry_min_diff", report.get("sentry_min_diff", 0), BAR),
        ("flamer_man_contrast", report["flamer_man_contrast"], CONTRAST_BAR),
        ("flamer_wand_px", report.get("flamer_wand_px", 0), PROPS["Flamer"][2]),
        ("sentry_plate_px", report.get("sentry_plate_px", 0), PROPS["Sentry"][2]),
    ]
    ok = report["unique_hashes"] is True
    for key, got, bar in bars:
        good = got >= bar
        ok = ok and good
        print("%-20s %-10s bar %-8s %s" % (key, got, bar, "ok" if good else "FAILS"))
    print("%-20s %-10s bar %-8s %s" % ("unique_hashes", report["unique_hashes"], True,
                                       "ok" if report["unique_hashes"] else "FAILS"))
    return 0 if ok else 1


if __name__ == "__main__":
    raise SystemExit(main())
