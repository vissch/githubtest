"""The probe for special-portraits-r2: are the Flamer's and the Sentry's portraits really their own pictures?

The harsh critic's finding was that Portraits/Flamer.png and Portraits/Sentry.png were byte-identical copies of
Shield.png while the commits called them his own portrait. A byte comparison alone would pass on a one-pixel change,
so this measures pixels: for each of the two, the SMALLEST mean absolute RGBA difference (0-1, everything resized to
256) against every OTHER portrait in the skin. A copy scores 0; a different picture scores well above the owner's
0.04 bar. It also records whether both sha1s are unique among the portraits, each cutout's opaque fraction (the
12-80 % bar of UiSkinVerifier.CheckPortrait), and the pose_error_m of the committed CaptureRig stills they were cut
from (the owner's evidence rule of 2026-10-19: < 0.5).

usage (from the repo root): python docs/evidence/special-portraits-r2/probe.py
Writes docs/evidence/special-portraits-r2/probe.json and exits 0 only when both diffs are >= 0.04 and both hashes
are unique.
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
SIZE = 256
BAR = 0.04


def load(path):
    return Image.open(path).convert("RGBA").resize((SIZE, SIZE), Image.LANCZOS)


def mean_abs_diff(a, b):
    pa, pb = a.getdata(), b.getdata()
    total = 0
    for x, y in zip(pa, pb):
        total += abs(x[0] - y[0]) + abs(x[1] - y[1]) + abs(x[2] - y[2]) + abs(x[3] - y[3])
    return total / float(SIZE * SIZE * 4 * 255)


def opaque_frac(path):
    im = Image.open(path).convert("RGBA")
    n = im.width * im.height
    return sum(1 for p in im.getdata() if p[3] > 32) / float(n)


def sha1(path):
    return hashlib.sha1(pathlib.Path(path).read_bytes()).hexdigest()


def pose_errors():
    """pose_error_m out of the CaptureRig sidecars committed beside this script."""
    out = {}
    for side in sorted(glob.glob(str(HERE / "*.json"))):
        if os.path.basename(side) == "probe.json":
            continue
        try:
            d = json.loads(pathlib.Path(side).read_text(encoding="utf-8"))
        except ValueError:
            continue
        if "pose_error_m" in d:
            out[os.path.basename(side)[:-5]] = float(d["pose_error_m"])
    return out


def main():
    files = sorted(glob.glob(str(PORTRAITS / "*.png")))
    imgs = {pathlib.Path(f).stem: load(f) for f in files}
    hashes = {pathlib.Path(f).stem: sha1(f) for f in files}
    placed = []
    if PLACEHOLDERS.exists():
        placed = [e.get("file") for e in json.loads(PLACEHOLDERS.read_text(encoding="utf-8")).get("files", [])]

    report = {
        "portraits": len(files),
        "bar_min_diff": BAR,
        "still_pose_error_m": pose_errors(),
        "listed_as_placeholder": {n: ("Portraits/%s.png" % n) in placed for n in MINE},
    }
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

    mine_hashes = [hashes.get(n) for n in MINE]
    report["unique_hashes"] = all(
        h is not None and sum(1 for o, oh in hashes.items() if oh == h) == 1 for h in mine_hashes
    )

    (HERE / "probe.json").write_text(json.dumps(report, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    print(json.dumps(report, indent=2, sort_keys=True))
    ok = (report.get("flamer_min_diff", 0) >= BAR and report.get("sentry_min_diff", 0) >= BAR
          and report["unique_hashes"] is True)
    return 0 if ok else 1


if __name__ == "__main__":
    raise SystemExit(main())
