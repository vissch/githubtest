"""Measures the DropInbound marker (flare + smoulder) at gameplay zoom 120, from a same-tick before/after pair
(weather held, CombatFx.ShowOverlays off, the event injected via Host.Events.Frame.Add — GymDirector.Preview's own
way to replay an event into presentation, never the sim). usage:
python docs/evidence/paradrop-inbound-marker-r3/dropprobe.py

Both shots are posed with focus_x/focus_z at the event's own position, so the projected zone centre is the frame's
centre (CaptureRig's sidecar JSON confirms this — read there, never assumed). The probe counts pixels inside a
60 px disc around that centre where the two frames differ by at least 100 (sum of the three channels' |delta|),
and the rise in p95 luma over the same disc (CaptureRig's own 0-1 Rec.709 scale, read from the PNG, not the sidecar,
so the two zooms are measured the same way).
"""
import json
import os
import sys
import numpy as np
from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
RADIUS = 60


def luma(rgb):
    r, g, b = rgb[..., 0], rgb[..., 1], rgb[..., 2]
    return 0.2126 * r + 0.7152 * g + 0.0722 * b


def disc_mask(h, w, cy, cx, radius):
    ys, xs = np.mgrid[0:h, 0:w]
    return (ys - cy) ** 2 + (xs - cx) ** 2 <= radius * radius


def measure(before_path, after_path):
    a = np.asarray(Image.open(before_path).convert("RGB")).astype(np.float32)
    b = np.asarray(Image.open(after_path).convert("RGB")).astype(np.float32)
    h, w, _ = a.shape
    cy, cx = h / 2.0, w / 2.0
    mask = disc_mask(h, w, cy, cx, RADIUS)

    diff = np.abs(b - a).sum(axis=2)
    changed_px = int(np.count_nonzero((diff >= 100.0) & mask))

    luma_a = luma(a / 255.0)[mask]
    luma_b = luma(b / 255.0)[mask]
    p95_up = float(np.percentile(luma_b, 95) - np.percentile(luma_a, 95))

    return changed_px, p95_up


def main():
    changed_120, up_120 = measure(os.path.join(HERE, "z120-before.png"), os.path.join(HERE, "z120-after.png"))
    changed_240, up_240 = measure(os.path.join(HERE, "z240-before.png"), os.path.join(HERE, "z240-after.png"))

    out = {
        "marker_changed_px_z120": changed_120,
        "marker_p95_luma_up": up_120,
        "marker_changed_px_z240": changed_240,
        "marker_p95_luma_up_z240": up_240,
    }
    out_path = os.path.join(HERE, "probe.json")
    with open(out_path, "w", encoding="utf-8") as f:
        json.dump(out, f, indent=2)
    print(json.dumps(out, indent=2))
    ok = changed_120 >= 400 and up_120 >= 0.1
    sys.exit(0 if ok else 1)


if __name__ == "__main__":
    main()
