#!/usr/bin/env python3
"""Count the pixels the Flamer's kit actually puts on the screen, from the stills.

WHY. The harsh critic gave flamer-look 14/100 on two findings: his kit (two fuel tanks and a wand,
CombatFx.Kit.cs) was never rendered at all, and where it is drawn it is drawn in dirtMat - dirt-coloured kit on
dirt-coloured ground, at night. Both claims are about pixels, so they are settled in pixels and not by eye.

The measurement is a DIFFERENCE between two frames of the SAME place: a Flamer stood on a flat spot, frozen
(SimHost.TimeScale = 0, CaptureRig.Hold), photographed; then an Assault stood on that same spot, frozen the same
way, photographed through the same camera pose. Everything that is not the kit - the night, the mud, the bunker
behind him, the baked soldier, the weather - is the same in both frames, so what is left in the difference is the
kit. A third frame with a SECOND Assault on the spot is the control: a measurement of two men who both wear
nothing must come out near zero, or the method is measuring noise.

The first way round was the plan's: both men in ONE frame, the Flamer's box against the same box shifted onto the
Assault beside him. Its control came out at 7075 differing pixels against a cap of 10, because the two boxes hold
two different pieces of BACKGROUND (one man in front of a bunker, the other in front of the mud bank), and the box
is 221x83 px at T3 with the man filling a fraction of it. The boxes were on the men - the overlays proved that -
but the method could not have measured kit. Hence one man at a time, on one spot.

INPUT (all written by stage.cs beside this file, in GreyboxCorridor, which is already Mood.Night / Biome.NightMud)
    flamer_t3.png / flamer_t2.png / flamer_t1.png     the Flamer on the spot, at the three view bands
    assault_t3.png / assault_t2.png / assault_t1.png  an Assault on the same spot, same bands, same camera
    control_t3.png                                    a second Assault on the spot, T3: the control
    <each>.json          CaptureRig's sidecar: pose_error_m, the camera pose, luma, contrast, men in frame
    boxes.json           per still, the SCREEN boxes of the tanks, the wand, the whole kit, a torso cube (chest
                         +-0.18 m) and a ground cube (0.6 m below his feet), projected by stage.cs through the very
                         camera pose CaptureRig photographs with. Normalised 0..1, y down from the top, so the
                         stills can be resampled before they are committed.

HOW
    kit_px            pixels inside the Flamer's kit box whose luma differs by >= 8 (0-255, Rec.601) between the
                      Flamer's still and the Assault's still of that band. The box is the SAME rectangle in both,
                      because both men stood on the same spot and the camera did not move.
    tank_vs_torso_dL  median luma of the Flamer's tank box minus the median luma of the SAME torso rectangle in
                      the bare Assault's still (the camera stands behind the man, so his own chest box is hidden
                      by the tanks), at T2. This is the
                      "dirt on dirt" number: a kit in dirtMat over a khaki soldier reads near 0.
    tank_vs_ground_dL the same against the ground box: does the kit separate from the mud he stands on.

OUTPUT  probe.json with exactly kit_px_t3, kit_px_t2, kit_px_control, tank_vs_torso_dL_t2, tank_vs_ground_dL_t2,
        a printed table, and <still>_boxes.png per still with every box drawn on it, so a human can see the boxes
        are on the man. If a box is off the man, the projection is wrong and no colour number means anything.

REFUSES (exit 1), never a silent zero: a missing still, a missing sidecar, pose_error_m >= 0.5 m, a box outside
        the frame, or two stills of a band that were not shot from the same camera pose.

PASS (--check), the plan's thresholds: kit_px_t3 >= 150, kit_px_t2 >= 40, kit_px_control <= 10,
        tank_vs_torso_dL_t2 >= 12, tank_vs_ground_dL_t2 >= 10.
"""
import json
import os
import sys

import numpy as np
from PIL import Image, ImageDraw

HERE = os.path.dirname(os.path.abspath(__file__))
DIFF = 8.0            # luma steps (0-255) that count as "these two frames do not agree on this pixel"
POSE_MAX = 0.5        # m; a still the camera did not reach is not evidence
SAME_CAM = 0.01       # m / degrees; the two stills of a band must be one camera pose, or the box does not line up
BANDS = ("t3", "t2", "t1")
THRESHOLDS = {"kit_px_t3": (150, "min"), "kit_px_t2": (40, "min"), "kit_px_control": (10, "max"),
              "tank_vs_torso_dL_t2": (12, "min"), "tank_vs_ground_dL_t2": (10, "min")}


def fail(msg):
    print("probe: " + msg)
    sys.exit(1)


def luma(path):
    a = np.asarray(Image.open(path).convert("RGB"), dtype=np.float32)
    return 0.299 * a[..., 0] + 0.587 * a[..., 1] + 0.114 * a[..., 2]


def sidecar(stem):
    p = os.path.join(HERE, stem + ".json")
    if not os.path.exists(p):
        fail("no sidecar for " + stem + " (CaptureRig writes it beside the png)")
    with open(p, encoding="utf-8") as fh:
        side = json.load(fh)
    err = float(side.get("pose_error_m", 9e9))
    if not err < POSE_MAX:
        fail("%s: pose_error_m %.3f >= %.1f - the camera did not reach the pose" % (stem, err, POSE_MAX))
    return side


def px_box(box, w, h):
    """A normalised box to integer pixels, with no clipping: a box off the frame is a bug, not a crop."""
    x0, y0, x1, y1 = box
    if min(x0, y0) < 0.0 or max(x1, y1) > 1.0 or x1 <= x0 or y1 <= y0:
        return None
    return (int(round(x0 * w)), int(round(y0 * h)), max(int(round(x1 * w)), int(round(x0 * w)) + 1),
            max(int(round(y1 * h)), int(round(y0 * h)) + 1))


def crop(a, b):
    return a[b[1]:b[3], b[0]:b[2]]


def same_camera(a, b):
    """The two stills of a band must come from one camera pose, or a shared box means nothing."""
    for key in ("cam_x", "cam_y", "cam_z", "cam_yaw", "cam_pitch"):
        if abs(float(a.get(key, 0.0)) - float(b.get(key, 1e9))) > SAME_CAM:
            return key
    return None


def overlay(stem, still, w, h):
    """Draw every box of a still on a copy of it, so a human can see they are on the man."""
    img = Image.open(os.path.join(HERE, still["png"])).convert("RGB")
    draw = ImageDraw.Draw(img)
    man = still["man"]
    for key, colour in (("tanks", (255, 90, 90)), ("wand", (255, 180, 80)), ("kit", (255, 255, 0)),
                        ("torso", (90, 160, 255)), ("ground", (120, 255, 120))):
        if key in man:
            b = px_box(man[key], w, h)
            if b:
                draw.rectangle(b, outline=colour)
    ax, ay = int(man["anchor"][0] * w), int(man["anchor"][1] * h)
    draw.line((ax - 8, ay, ax + 8, ay), fill=(255, 255, 255))
    draw.line((ax, ay - 8, ax, ay + 8), fill=(255, 255, 255))
    # 128 colours: an overlay is there to be looked at, not measured, and seven full-colour ones were 6.7 MB of
    # repository for pictures nothing reads back.
    img.quantize(colors=128).save(os.path.join(HERE, stem + "_boxes.png"), optimize=True)


def kit_px(band, with_kit, without, detail):
    """Pixels of the Flamer's kit box that the two frames of this band do not agree on."""
    a_stem, a_still = with_kit
    b_stem, b_still = without
    for stem, still in ((a_stem, a_still), (b_stem, b_still)):
        if not os.path.exists(os.path.join(HERE, still["png"])):
            fail("missing still " + still["png"])
    moved = same_camera(sidecar(a_stem), sidecar(b_stem))
    if moved:
        fail("%s and %s were not shot from the same camera pose (%s differs)" % (a_stem, b_stem, moved))
    A = luma(os.path.join(HERE, a_still["png"]))
    B = luma(os.path.join(HERE, b_still["png"]))
    if A.shape != B.shape:
        fail("%s and %s are different sizes" % (a_stem, b_stem))
    h, w = A.shape
    if "kit" not in a_still["man"]:
        fail(a_stem + ": boxes.json has no kit box (CombatFx.KitPieces filled nothing)")
    b = px_box(a_still["man"]["kit"], w, h)
    if b is None:
        fail(a_stem + ": the kit box falls outside the frame")
    pa, pb = crop(A, b), crop(B, b)
    if pa.shape[0] < 2 or pa.shape[1] < 2:
        fail(a_stem + ": the kit box is smaller than 2x2 px")
    n = int(np.count_nonzero(np.abs(pa - pb) >= DIFF))
    overlay(a_stem, a_still, w, h)
    overlay(b_stem, b_still, w, h)
    detail[band] = {"a": a_stem, "b": b_stem, "box_px": [pa.shape[1], pa.shape[0]], "kit_px": n}
    return n


def dls(still, bare):
    """tank_vs_torso and tank_vs_ground for one still, in luma steps (0-255).

    The two stills are the SAME man in the SAME slot one frame apart, his archetype flipped (stage.cs): every pixel
    that differs between them is kit and nothing else. So the tanks are measured on exactly those pixels inside the
    tank box - not on the whole box, which is mostly the man and the mud behind him and whose median moved by 1 luma
    step when the kit went from mud brown to pale steel (measured 2026-10-19).

      tanks  median luma of the changed pixels in the Flamer's still   - the kit as it is drawn
      torso  median luma of the torso box (his chest cube, +-0.18 m) in the bare Assault's still - the man the kit
             is worn over. Read off the bare man on purpose: the camera stands behind him, where the tanks are, so
             in the Flamer's own still that box is the kit and the number would measure the kit against itself.
      ground median luma of the ground box, 0.6 m below his feet, in the Flamer's still
    """
    L = luma(os.path.join(HERE, still["png"]))
    B = luma(os.path.join(HERE, bare["png"]))
    h, w = L.shape
    man = still["man"]
    for key in ("tanks", "torso", "ground"):
        if key not in man:
            fail("boxes.json has no " + key + " box")
    tb = px_box(man["tanks"], w, h)
    gb = px_box(man["ground"], w, h)
    sb = px_box(man["torso"], w, h)
    if tb is None or gb is None or sb is None:
        fail("the tank, torso or ground box falls outside the frame")
    a, b = crop(L, tb), crop(B, tb)
    mask = np.abs(a - b) >= DIFF
    if int(np.count_nonzero(mask)) < 20:
        fail("fewer than 20 px of the tank box changed when the man put the kit on: there is no kit to measure")
    medians = {"tanks": float(np.median(a[mask])), "torso": float(np.median(crop(B, sb))),
               "ground": float(np.median(crop(L, gb)))}
    return medians["tanks"] - medians["torso"], medians["tanks"] - medians["ground"], medians


def main():
    check = "--check" in sys.argv
    with open(os.path.join(HERE, "boxes.json"), encoding="utf-8") as fh:
        boxes = json.load(fh)
    for part in ("flamer", "assault", "control"):
        if part not in boxes:
            fail("boxes.json has no " + part + " part (stage.cs, merge phase)")
    flamer, assault, control = boxes["flamer"]["stills"], boxes["assault"]["stills"], boxes["control"]["stills"]
    detail, result = {}, {}
    for band in BANDS:
        a, b = "flamer_" + band, "assault_" + band
        if a not in flamer or b not in assault:
            fail("boxes.json is missing " + a + " or " + b)
        n = kit_px(band, (a, flamer[a]), (b, assault[b]), detail)
        if band in ("t3", "t2"):
            result["kit_px_" + band] = n
    if "control_t3" not in control:
        fail("boxes.json has no control_t3")
    # The control is measured over the Flamer's own T3 kit box, the same rectangle as kit_px_t3, so the two numbers
    # are the same measurement on the same pixels - one with a kit in it, one with two bare Assaults.
    pair = dict(control["control_t3"])
    pair["man"] = dict(pair["man"])
    pair["man"]["kit"] = flamer["flamer_t3"]["man"]["kit"]
    assault_t3 = dict(assault["assault_t3"])
    assault_t3["man"] = dict(assault_t3["man"])
    assault_t3["man"]["kit"] = flamer["flamer_t3"]["man"]["kit"]
    result["kit_px_control"] = kit_px("control", ("assault_t3", assault_t3), ("control_t3", pair), detail)
    torso, ground, medians = dls(flamer["flamer_t2"], assault["assault_t2"])
    result["tank_vs_torso_dL_t2"] = round(torso, 2)
    result["tank_vs_ground_dL_t2"] = round(ground, 2)

    with open(os.path.join(HERE, "probe.json"), "w", encoding="utf-8") as fh:
        json.dump(result, fh, indent=2, sort_keys=True)
        fh.write("\n")

    print("%-22s %10s %10s %s" % ("number", "measured", "needs", "verdict"))
    bad = []
    for key, (want, how) in THRESHOLDS.items():
        got = result[key]
        ok = got >= want if how == "min" else got <= want
        print("%-22s %10s %10s %s" % (key, got, ("<=" if how == "max" else ">=") + str(want), "ok" if ok else "FAIL"))
        if not ok:
            bad.append(key)
    print("")
    print("T2 medians (luma 0-255): tanks %.1f  torso %.1f  ground %.1f"
          % (medians["tanks"], medians["torso"], medians["ground"]))
    for band, d in sorted(detail.items()):
        print("%-8s %s vs %s over %sx%s px: %s differ"
              % (band, d["a"], d["b"], d["box_px"][0], d["box_px"][1], d["kit_px"]))
    print("overlays: " + ", ".join(sorted(set(d["a"] for d in detail.values()) | set(d["b"] for d in detail.values()))))
    if check and bad:
        print("")
        print("probe: not there yet: " + ", ".join(bad))
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
