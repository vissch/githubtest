#!/usr/bin/env python3
"""Measures the three pairs capture.py wrote: one frame per seed holding an Engineer and (after the sanctioned
SimHost.WriteWorlds flip) the same two men with one of them a Sapper, framed and measured from the engine's own
boxes (CaptureRig.Measure's new `men` array), never a hand-typed crop.

Seeds 1 and 2 hold both men readable (CaptureRig's own contrast != -1, tall >= 120 px) in every shot: the crop
used for every comparison is the RIGHT man's own footprint box, read straight from the sidecar that was taken
alongside each PNG. kit_frac is how much of that box changed under the Engineer -> Sapper flip; noise_frac is how
much of it changed between two repeat shots of the SAME still-Engineer pose (noise-N.png), the pure measurement
floor docs/evidence/sapper-look-3/README.md found from an unbroken Hold. The bar sits between the two.

Seed 3 is the owner's own zoom-7 view among the stakes. At that zoom the margin (CaptureRig.Measure: the ring has
to sit inside the image) only ever holds ONE of the two men readable at a time in every pose this script tried -
the risk the plan called out in advance. It is reported, not gated on the same bar: there is no Engineer/Sapper
comparison to make when the flipped man is the one standing outside the frame.
"""
import json
import os

from PIL import Image

HERE = os.path.dirname(os.path.abspath(__file__))
THRESHOLD = 10          # per-channel abs diff above which a pixel counts as "changed" (docs/evidence/sapper-look-3)
BAR = 0.01               # the box must change more than this fraction of its pixels to count as a real kit change


def load(name):
    return Image.open(os.path.join(HERE, name)).convert("RGB")


def sidecar(name):
    with open(os.path.join(HERE, name.replace(".png", ".json")), "r", encoding="utf-8") as f:
        return json.load(f)


def diff_frac(img_a, img_b, box):
    a = img_a.crop(tuple(box)).load()
    b = img_b.crop(tuple(box)).load()
    w = box[2] - box[0]
    h = box[3] - box[1]
    hits = 0
    for y in range(h):
        for x in range(w):
            ra, ga, ba = a[x, y]
            rb, gb, bb = b[x, y]
            if max(abs(ra - rb), abs(ga - gb), abs(ba - bb)) > THRESHOLD:
                hits += 1
    return hits / (w * h)


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
    eng_img, sap_img, noise_img = load(f"pair-engineer-{n}.png"), load(f"pair-sapper-{n}.png"), load(f"noise-{n}.png")
    eng_men, sap_men = men_by_slot(sidecar(f"pair-engineer-{n}.png")), men_by_slot(sidecar(f"pair-sapper-{n}.png"))
    slot = flipped_slot(eng_men, sap_men)
    if slot is None:
        return None
    box = eng_men[slot]["box"]
    return {
        "seed": n, "slot": slot, "box": box, "both_readable": len(eng_men) == 2 and len(sap_men) == 2,
        "tall": [m["tall"] for m in eng_men.values()] + [m["tall"] for m in sap_men.values()],
        "contrast": [m["contrast"] for m in eng_men.values()] + [m["contrast"] for m in sap_men.values()],
        "kit_frac": diff_frac(sap_img, eng_img, box),
        "noise_frac": diff_frac(noise_img, eng_img, box),
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
            print(f"FAIL seed: no readable Engineer->Sapper flip with both men in frame")
            failed = True
    seed3 = seed3_report()

    tall_all = [t for p in pairs if p for t in p["tall"]] + seed3["tall"]
    contrast_all = [c for p in pairs if p for c in p["contrast"]] + seed3["contrast"]
    kit_fracs = [p["kit_frac"] for p in pairs if p]
    noise_fracs = [p["noise_frac"] for p in pairs if p]

    probe = {
        "pairs": [p for p in pairs if p],
        "seed3_single_man": seed3,
        "tall_min": min(tall_all) if tall_all else -1,
        "contrast_min": min(contrast_all) if contrast_all else -1,
        "kit_frac_min": min(kit_fracs) if kit_fracs else -1,
        "noise_frac_max": max(noise_fracs) if noise_fracs else -1,
        "bar": BAR,
    }
    with open(os.path.join(HERE, "probe.json"), "w", encoding="utf-8") as f:
        json.dump(probe, f, indent=2)
    print(json.dumps(probe, indent=2))

    if probe["tall_min"] < 120:
        print(f"FAIL tall_min {probe['tall_min']} < 120")
        failed = True
    # the two-man pairs (seeds 1, 2) are the only ones with a kit to read; 0.12 is CaptureRig's own
    # "men barely separate from the ground: unreadable" warning threshold (Measure's `warn` list)
    pair_contrasts = [c for p in pairs if p for c in p["contrast"]]
    if pair_contrasts and min(pair_contrasts) < 0.12:
        print(f"FAIL contrast_min (seeds 1, 2) {min(pair_contrasts)} < 0.12")
        failed = True
    if not (probe["noise_frac_max"] < probe["bar"] <= probe["kit_frac_min"]):
        print(f"FAIL bar: noise_frac_max={probe['noise_frac_max']} bar={probe['bar']} kit_frac_min={probe['kit_frac_min']}")
        failed = True
    return 1 if failed else 0


if __name__ == "__main__":
    raise SystemExit(main())
