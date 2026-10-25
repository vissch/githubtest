# Visual review: eleven rounds of the gas bank, side by side

The owner's answer on the Decide page to "ability-fx-captures-r2-2: chain of fix units 10 deep, 26 units" was
"show me visual results". These four pictures are that: no look or code changed in this leg, only the
eleven existing captures assembled into sheets.

## The pictures

- `chain-strip.jpg` — all eleven t1 captures, full frame, 3x4 grid, oldest first. Each panel is labelled
  "R\<n\> \<what that build changed\>" plus the `probe.json` keys that round's `run1` holds (early rounds carry
  fewer keys; the probe grew new measures as the lane went on).
- `chain-zoom.jpg` — the same eleven panels, cropped to a fixed box on the bank (`(80, 180, 1120, 680)` of the
  1280x720 frame, tuned by eye) so the gas is not a smudge at this scale.
- `round01-no-bank.jpg` — round 1 alone, the "no bank at all" still, full frame.
- `round11-latest.jpg` — round 11 alone, the newest capture, full frame.
- `strip.json` — the eleven sources in order, machine-readable.

## The eleven sources, oldest first

| round | commit | source | what that build changed |
|---|---|---|---|
| 1 | `03c540df1c` | `docs/evidence/ability-fx-captures-r2/gas-t1.jpg` | gas stills harness; no bank at all |
| 2 | `843a03570c` | `docs/evidence/ability-fx-captures-r2-2-g2/run1/gas-t1.jpg` | first bank, three-capture probe bars set |
| 3 | `0dc028a72b` | `docs/evidence/ability-fx-captures-r2-2-g2-g1/run1/gas-t1.jpg` | wide overlapping cards, a probe that can see the zebra |
| 4 | `96a8be5d66` | `docs/evidence/ability-fx-captures-r2-2-g2-g1-g2/run1/gas-t1.jpg` | bar the paint in the frame, three runs on three frames |
| 5 | `7a5cdd26cf` | `docs/evidence/ability-fx-captures-r2-2-g2-g1-g2-g2/run1/gas-t1.jpg` | bar the paint by the direction it lifts, not its hue |
| 6 | `c32d7dd90a` | `docs/evidence/ability-fx-captures-r2-2-g2-g1-g2-g2-g2/run1/gas-t1.jpg` | reject a plain night card with the paint's own green lean |
| 7 | `394d617b90` | `docs/evidence/ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1/run1/gas-t1.jpg` | three captures of the new build, every bar re-set |
| 8 | `4b50e2a26a` | `docs/evidence/ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g1/run1/gas-t1.jpg` | closed-gap build, paint-share floor at 0.70 |
| 9 | `4abe853d3d` | `docs/evidence/ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g1-g1/run1/gas-t1.jpg` | jitter widened to a full field cell |
| 10 | `3dca38050a` | `docs/evidence/ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g1-g1-g2/run1/gas-t1.jpg` | stratified jitter (thirds of the cell), every bar re-set |
| 11 | `1a3da36a4a` | `docs/evidence/ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g1-g1-g2-g2/moved-run1/gas-t1.jpg` | gas centre moved off the ditch — **reverted**, kept only as proof |

Panel 11 is the reverted moved-centre build: both levers tried against the owner's "dark row is the flooded ditch"
complaint were rebutted by measurement (`../README.md`), and the centre move was reverted afterward. The shipped
build at the old centre (thirds-of-the-cell jitter, `59fcb4c9b4`) has never itself been photographed — that is
named as the cheapest open work in `../README.md`, not done here.

## Scope

No look, constant, or code changed in this leg. These are pictures of captures that already existed.
