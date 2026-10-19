#!/usr/bin/env python
"""Measure the stills shoot.py shot, in pixels (sentry-look-r4, step 6).

The harsh critic's fix 2 on sentry-look-r2 was that its `plate_dead.json` read `drawn_infantry 1, fallen 0`: the
plate did survive the Despawn, but no death had ever played, so "the plate outlives his death" was never shown. And
the plate had only ever been seen as a flat magenta mask, never in the night look.

So this folder's stills come from a Sentry killed by the sim's own Death event - SimWorld.Despawn with a killer slot,
collected into the pump and dispatched, so CombatFx.OnDeath runs and the fallen body is added - and the measurement is
in pixels of those stills, not arithmetic:

    fallen_after_death           VATRenderer.FallenCount in plate_dead's sidecar   >= 1 passes
    drawn_infantry_after_death   VATRenderer.DrawnInfantry there                   == 0 passes
    plate_px_t                   magenta pixels of plate_t.png   (alive, firing)
    plate_px_after_death         magenta pixels of plate_dead.png (after the death) within 2 % of plate_px_t

CombatFx.PlateMask draws the plate and its post in flat magenta and, standing on the same ground as the post, a
calibration bar exactly 0.50 m tall in flat cyan. That bar turns pixels into metres in the same still, under the
same camera, so no focal length, projection or camera pose has to be trusted. night_dead.png is the same frame with
the mask off - the unmasked T3 night still - and only its mean luma is reported, as a sanity check that something
was actually lit and rendered.

Both masked stills are the same frozen instant of the same battle from the same camera (shoot.py stops the clock
before either of them and never starts it again), so the only difference between them is his death. That is what
lets a 2 per cent band mean "the plate did not move": nothing else could have moved it.

Run from the repo root:  python docs/evidence/sentry-look-r4/probe.py
Writes probe.json beside the stills. Exit 0 when the three thresholds pass, 1 when any fails.
"""
import json
import os
import sys

import numpy as np
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
BAR_M = 0.50          # the calibration bar's height in metres (CombatFx.PlateBar)
TOL = 40              # per-channel tolerance out of 255: the mask is unlit and flat, so it needs no more
MAGENTA = (255, 0, 255)
CYAN = (0, 255, 255)

BAND = 0.02           # the plate may differ by 2 % of its live pixel count, no more (the owner's bar)
MASKED = ("plate_t", "plate_dead")
NIGHT = "night_dead"


def mask(rgb, want):
    """Where the image is within TOL of `want` on every channel."""
    d = np.abs(rgb.astype(np.int16) - np.array(want, dtype=np.int16))
    return (d <= TOL).all(axis=2)


def extent(m):
    """count, top row, bottom row, and the bounding-box centre (row, col); rows count down from the top."""
    rows, cols = np.nonzero(m)
    if rows.size == 0:
        return dict(count=0, top=None, bottom=None, centre=None)
    return dict(count=int(rows.size), top=int(rows.min()), bottom=int(rows.max()),
                centre=[(float(rows.min()) + float(rows.max())) / 2.0,
                        (float(cols.min()) + float(cols.max())) / 2.0])


def pixels(stem):
    path = os.path.join(HERE, stem + ".png")
    if not os.path.exists(path):
        raise SystemExit("missing still: " + path)
    rgb = np.asarray(Image.open(path).convert("RGB"))
    return rgb, dict(file=stem + ".png", size=[int(rgb.shape[1]), int(rgb.shape[0])],
                     magenta=extent(mask(rgb, MAGENTA)), cyan=extent(mask(rgb, CYAN)))


def sidecar(stem):
    path = os.path.join(HERE, stem + ".json")
    if not os.path.exists(path):
        raise SystemExit("missing sidecar: " + path)
    with open(path, encoding="utf-8") as f:
        return json.load(f)


def counter(side, *names):
    """A CaptureRig sidecar counter, wherever the rig put it (top level, or under a nested dict)."""
    for n in names:
        if n in side:
            return side[n]
    for v in side.values():
        if isinstance(v, dict):
            for n in names:
                if n in v:
                    return v[n]
    return None


def main():
    shots = {}
    for s in MASKED:
        _, shots[s] = pixels(s)
    t, dead = shots["plate_t"], shots["plate_dead"]

    bar = t["cyan"]
    if not bar["count"] or bar["bottom"] == bar["top"]:
        raise SystemExit("no calibration bar in plate_t: is CombatFx.PlateMask on, and the plate in frame?")
    px_per_m = (bar["bottom"] - bar["top"]) / BAR_M

    plate = t["magenta"]
    if not plate["count"]:
        raise SystemExit("no plate in plate_t: nothing to measure")
    # the post's bottom should sit on the bar's bottom. Magenta reaching below the cyan is the post's front face
    # standing 0.04 m nearer the camera than the thin bar, not a gap, so only magenta stopping short counts.
    foot_gap_m = max(0.0, (bar["bottom"] - plate["bottom"]) / px_per_m)

    plate_px_t = plate["count"]
    plate_px_after_death = dead["magenta"]["count"]

    side_t, side_dead = sidecar("plate_t"), sidecar("plate_dead")
    fallen_before = counter(side_t, "fallen", "fallen_count", "fallen_men")
    fallen_after = counter(side_dead, "fallen", "fallen_count", "fallen_men")
    drawn_before = counter(side_t, "drawn_infantry", "men_in_frame", "drawn")
    drawn_after = counter(side_dead, "drawn_infantry", "men_in_frame", "drawn")

    night_rgb, night = pixels(NIGHT)
    luma = float(np.asarray(night_rgb, dtype=np.float64).mean())

    kill_path = "unknown"
    kp = os.path.join(HERE, "kill_path.txt")
    if os.path.exists(kp):
        kill_path = open(kp, encoding="utf-8").read().strip() or "unknown"

    out = {
        "fallen_after_death": fallen_after,
        "drawn_infantry_after_death": drawn_after,
        "plate_px_t": plate_px_t,
        "plate_px_after_death": plate_px_after_death,
        "fallen_before_death": fallen_before,
        "drawn_infantry_before_death": drawn_before,
        "px_per_m": round(px_per_m, 2),
        "foot_gap_m": round(foot_gap_m, 4),
        "kill_path": kill_path,
        "luma_mean": round(luma, 3),
        "bar_m": BAR_M,
        "tolerance_per_channel": TOL,
        "thresholds": {"fallen_after_death": ">= 1", "drawn_infantry_after_death": "== 0",
                       "plate_px_after_death": "within %g%% of plate_px_t" % (BAND * 100.0)},
        "shots": dict(shots, **{NIGHT: night}),
        "pose_error_m": {s: sidecar(s).get("pose_error_m") for s in (MASKED + (NIGHT,))},
    }
    drift = abs(plate_px_after_death - plate_px_t)
    out["plate_px_drift"] = int(drift)
    out["pass"] = bool((fallen_after or 0) >= 1 and drawn_after == 0 and drift <= BAND * plate_px_t)

    with open(os.path.join(HERE, "probe.json"), "w", encoding="utf-8") as f:
        json.dump(out, f, indent=2)
        f.write("\n")

    ok = lambda b: "pass" if b else "FAIL"
    print("px_per_m                   %8.2f  (cyan bar %d px for %.2f m)"
          % (px_per_m, bar["bottom"] - bar["top"], BAR_M))
    print("foot_gap_m                 %8.4f  (context)" % foot_gap_m)
    print("kill_path                  %8s" % kill_path)
    print("fallen_before_death        %8s" % fallen_before)
    print("drawn_infantry_before      %8s" % drawn_before)
    print("fallen_after_death         %8s  (>= 1)      %s" % (fallen_after, ok((fallen_after or 0) >= 1)))
    print("drawn_infantry_after_death %8s  (== 0)      %s" % (drawn_after, ok(drawn_after == 0)))
    print("plate_px_t                 %8d" % plate_px_t)
    print("plate_px_after_death       %8d  (+-%d px)   %s"
          % (plate_px_after_death, int(BAND * plate_px_t), ok(drift <= BAND * plate_px_t)))
    print("night luma_mean            %8.3f  (context)" % luma)
    return 0 if out["pass"] else 1


if __name__ == "__main__":
    sys.exit(main())
