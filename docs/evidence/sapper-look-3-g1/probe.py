#!/usr/bin/env python3
"""Measures the stills capture.py wrote: per seed one frame holding an Engineer, one holding the same two men with
one of them flipped to a Sapper (the sanctioned SimHost.WriteWorlds flip), and three repeat captures of the
*unchanged* Engineer build that are the control. Framing and boxes come from the engine's own measurement
(CaptureRig.Measure's `men` array), never a hand-typed crop.

Every number here is one measurement, `aligned_frac`. The window's SIZE is fixed once, from the Engineer still's
own box for the flipped slot; its ANCHOR is each frame's own centre column and foot row, read from that frame's own
sidecar. A man who walked or breathed between two captures is therefore compared to himself, not to the ground he
left; his kit is still in the count. (The whole-box diff the earlier leg used counted translation as change, which
is why seed 1 read 0.004 - the window had drifted off the man.)

- `kit_frac` - the aligned Engineer -> Sapper diff: the kit change we want to see.
- `spread_max` - the control: the largest pairwise aligned diff among the three captures of the current build
  (pair-engineer-N, noise-N, noise-N-b), all three taken the way the kit shot is taken. This is the honest
  measurement floor, in the owner's words: the bar has to sit outside the spread of three captures of the build.
- the `_band` numbers beside each: the same measurement restricted to the kit band - the top quarter of the window
  plus the outer 22 % of its columns, which is where CombatFx.SapperKit.cs:37-42 puts the pack above the shoulder
  line and the two charges past the torso's width.

Seed 3 is the owner's own zoom-7 view among the stakes; at that zoom CaptureRig's margin rule only ever holds one
of the two men readable, so it is reported, not compared.

BAR is left where the previous leg had it on purpose: this part of the work only makes the measurement honest. The
probe is expected to stay red until the capture side (the released window between repeat shots, and the corner
search's own noise gate) is fixed.
"""
import json
import os

from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
THRESHOLD = 10          # per-channel abs diff above which a pixel counts as "changed" (docs/evidence/sapper-look-3)
BAR = 0.01              # the window must change more than this fraction of its pixels to count as a real kit change
BAND_TOP = 0.25         # top quarter of the window: the pack above the shoulder
BAND_SIDE = 0.22        # outer fraction of the columns on each side: the charges past the torso

_WINDOW = {}            # slot -> (w, h), the window size taken once from the Engineer still


def load(name):
    return Image.open(os.path.join(HERE, name)).convert("RGB")


def sidecar(name):
    with open(os.path.join(HERE, name.replace(".png", ".json")), "r", encoding="utf-8") as f:
        return json.load(f)


def man(data, slot):
    for m in data.get("men", []):
        if m["slot"] == slot:
            return m
    return None


def anchored_rect(png, slot, w, h):
    """The window of size (w, h) hung on this frame's OWN anchor: its centre column and its foot row."""
    m = man(sidecar(png), slot)
    if m is None:
        return None
    x0 = int(round(m["cx"])) - w // 2
    y1 = int(m["box"][3])
    return (x0, y1 - h, x0 + w, y1)


def in_band(x, y, w, h):
    return y < h * BAND_TOP or x < w * BAND_SIDE or x >= w * (1.0 - BAND_SIDE)


def aligned_frac(png_a, png_b, slot, band=False):
    """The fraction of the aligned window that differs between two frames. The one measurement both this probe and
    capture.py's corner search use, so a corner is judged by the same rule that will judge the evidence."""
    w, h = _WINDOW[slot]
    ra, rb = anchored_rect(png_a, slot, w, h), anchored_rect(png_b, slot, w, h)
    if ra is None or rb is None:
        return None
    img_a, img_b = load(png_a), load(png_b)
    W, H = img_a.size
    # clip each window to its own image, then keep the intersection of the two clips so both crops are one size
    cut_l = max(0, -ra[0], -rb[0])
    cut_t = max(0, -ra[1], -rb[1])
    cut_r = max(0, ra[2] - W, rb[2] - W)
    cut_b = max(0, ra[3] - H, rb[3] - H)
    cw, ch = w - cut_l - cut_r, h - cut_t - cut_b
    if cw <= 0 or ch <= 0:
        return None
    a = img_a.crop((ra[0] + cut_l, ra[1] + cut_t, ra[2] - cut_r, ra[3] - cut_b)).load()
    b = img_b.crop((rb[0] + cut_l, rb[1] + cut_t, rb[2] - cut_r, rb[3] - cut_b)).load()
    hits = 0
    counted = 0
    for y in range(ch):
        for x in range(cw):
            if band and not in_band(x + cut_l, y + cut_t, w, h):
                continue
            counted += 1
            r1, g1, b1 = a[x, y]
            r2, g2, b2 = b[x, y]
            if max(abs(r1 - r2), abs(g1 - g2), abs(b1 - b2)) > THRESHOLD:
                hits += 1
    return hits / counted if counted else None


def men_by_slot(data):
    return {m["slot"]: m for m in data.get("men", []) if m.get("contrast", -1) != -1}


def flipped_slot(eng_men, sap_men):
    """The slot SimHost.WriteWorlds actually changed archetype on: present and readable in BOTH shots (not merely
    one of them, which would be drift, not the flip), with a different archetype in each."""
    for slot, e in eng_men.items():
        s = sap_men.get(slot)
        if s is not None and s["archetype"] != e["archetype"]:
            return slot
    return None


def pair_measure(n):
    eng, sap = f"pair-engineer-{n}.png", f"pair-sapper-{n}.png"
    eng_men, sap_men = men_by_slot(sidecar(eng)), men_by_slot(sidecar(sap))
    slot = flipped_slot(eng_men, sap_men)
    if slot is None:
        return None
    box = eng_men[slot]["box"]
    _WINDOW[slot] = (box[2] - box[0], box[3] - box[1])

    control = [c for c in (eng, f"noise-{n}.png", f"noise-{n}-b.png")
               if os.path.exists(os.path.join(HERE, c))]
    spread, spread_band = [], []
    for i in range(len(control)):
        for j in range(i + 1, len(control)):
            f = aligned_frac(control[i], control[j], slot)
            if f is not None:
                spread.append(f)
            f = aligned_frac(control[i], control[j], slot, band=True)
            if f is not None:
                spread_band.append(f)
    return {
        "seed": n, "slot": slot, "window": _WINDOW[slot],
        "both_readable": len(eng_men) == 2 and len(sap_men) == 2,
        "tall": [m["tall"] for m in eng_men.values()] + [m["tall"] for m in sap_men.values()],
        "contrast": [m["contrast"] for m in eng_men.values()] + [m["contrast"] for m in sap_men.values()],
        "kit_frac": aligned_frac(sap, eng, slot),
        "kit_frac_band": aligned_frac(sap, eng, slot, band=True),
        "control_shots": control,
        "spread_max": max(spread) if spread else None,
        "spread_max_band": max(spread_band) if spread_band else None,
        "spread_all": spread,
    }


def seed3_report():
    eng_men = men_by_slot(sidecar("pair-engineer-3.png"))
    sap_men = men_by_slot(sidecar("pair-sapper-3.png"))
    return {
        "readable_count_engineer": len(eng_men), "readable_count_sapper": len(sap_men),
        "tall": [m["tall"] for m in eng_men.values()] + [m["tall"] for m in sap_men.values()],
        "contrast": [m["contrast"] for m in eng_men.values()] + [m["contrast"] for m in sap_men.values()],
    }


def main():
    pairs = [pair_measure(1), pair_measure(2)]
    failed = False
    for p in pairs:
        if p is None or not p["both_readable"]:
            print("FAIL seed: no readable Engineer->Sapper flip with both men in frame")
            failed = True
    seed3 = seed3_report()

    tall_all = [t for p in pairs if p for t in p["tall"]] + seed3["tall"]
    contrast_all = [c for p in pairs if p for c in p["contrast"]] + seed3["contrast"]
    kit_fracs = [p["kit_frac"] for p in pairs if p and p["kit_frac"] is not None]
    spreads = [p["spread_max"] for p in pairs if p and p["spread_max"] is not None]

    probe = {
        "pairs": [p for p in pairs if p],
        "seed3_single_man": seed3,
        "tall_min": min(tall_all) if tall_all else -1,
        "contrast_min": min(contrast_all) if contrast_all else -1,
        "kit_frac_min": min(kit_fracs) if kit_fracs else -1,
        "spread_max": max(spreads) if spreads else -1,
        "bar": BAR,
    }
    with open(os.path.join(HERE, "probe.json"), "w", encoding="utf-8") as f:
        json.dump(probe, f, indent=2)
    print(json.dumps(probe, indent=2))

    for p in probe["pairs"]:
        print(f"seed {p['seed']}: kit_frac={p['kit_frac']} (band {p['kit_frac_band']})  "
              f"spread_max={p['spread_max']} (band {p['spread_max_band']})")

    if probe["tall_min"] < 120:
        print(f"FAIL tall_min {probe['tall_min']} < 120")
        failed = True
    # the two-man pairs (seeds 1, 2) are the only ones with a kit to read; 0.12 is CaptureRig's own
    # "men barely separate from the ground: unreadable" warning threshold (Measure's `warn` list)
    pair_contrasts = [c for p in pairs if p for c in p["contrast"]]
    if pair_contrasts and min(pair_contrasts) < 0.12:
        print(f"FAIL contrast_min (seeds 1, 2) {min(pair_contrasts)} < 0.12")
        failed = True
    if not (probe["spread_max"] < probe["bar"] <= probe["kit_frac_min"]):
        print(f"FAIL bar: spread_max={probe['spread_max']} bar={probe['bar']} kit_frac_min={probe['kit_frac_min']}")
        failed = True
    return 1 if failed else 0


if __name__ == "__main__":
    raise SystemExit(main())
