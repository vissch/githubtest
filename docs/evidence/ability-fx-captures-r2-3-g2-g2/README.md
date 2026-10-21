# The chlorine bank, round 3-g2-g2: the upright face in pixels, and one hue

The owner (2026-10-21): "`gas_side_height_m` stays inside 2.798 to 2.888 while `side_blob_px` runs 83100, 210655
and 303462, because the number is the silhouette top minus a 10 m footprint bias. `o240_hue_dist_deg` is 74.81,
100.89 and 123.02, and the bar of 69.8 sits 5 degrees under that scatter. Measure the upright face in pixels, and
make the three seeds agree on one hue, with both bars outside the spread of three captures of the current build."

This folder answers both halves: **the face measure** (`side_face_px`) and **the hue reading** (the core, read
with a saturation-weighted median), each with its bar outside the spread of three captures of this build. No
drawn look changed: both complaints were about what the probe measures.

## Why 2.80-2.89 m was blind

`gas_side_height_m` is one median column's silhouette top minus `footprint_bias_m` at that column's distance.
Both halves are set by the POSE, not by the bank: the side band always shoots from 7.8 m up at pitch 8, so the
top of a card that reaches the same screen height always reads about the same metres, and the bias subtracted
from it is a fixed function of distance. So the reading held 2.817 / 2.798 / 2.888 m across three seeds whose
side blob ran **83100 / 210655 / 303462 pixels** - the bank tripled and the barred number moved 0.09 m. A bar on
it polices the pose, not the cloud.

## The face measure (`side_face_px`)

New helper `face_px`. Per side-band blob column with at least `MIN_COLUMN_PX` pixels:

1. the column's base distance, taken exactly as `measure_side` does it - `ground_distance` of the BOTTOM pixel,
2. `footprint_bias_m` at that distance: the highest a patch of ground sloping away beyond the base can read,
3. **count** the column's pixels whose `height_at(..., dist)` is above that bias.

Ground beyond the base can never read above its own bias, so those pixels are not the field sloping away: they
are the bank's own upright body on screen. `side_face_px` is the total over columns - it grows when the bank
grows (52k / 135k / 186k against blobs of 83k / 210k / 303k). Two size-free rows sit beside it:
`side_face_frac` (face / blob: the share of the silhouette that stands up, 0.615-0.643 here) and
`side_face_col_px` (the median column, 109-143 px).

`gas_side_height_m` is kept as a REPORTED row, with `side_top_m` and `side_bias_m`: the complaint is that it was
barred and blind, not that it is useless. `side_pose_bad` still fails the probe (exit 1) if a side sidecar is not
within 1 deg of pitch 8 and 1.5 m of 7.8 m above ground - a face in pixels only means anything at one pose.

`selftest()` checks the face by plain trigonometry, worked outside the code it tests: camera (0, 10, 0), pitch 45,
ground y = 0, base distance 10 m, so the bias is 3.3333 m. A point that high, 10 m out, sits at depression
`atan(6.6667/10)` = 33.690 deg, 11.310 deg above the centre ray, so `ndc_y` = `tan(11.310)/tan(12.5)` =
0.2000/0.22169 = 0.9022, which on a 720-row frame is row `((1-0.9022)/2)*720 - 0.5` = 34.7. A synthetic single
centre column holding rows 0..100 (plus the centre row, the bottom pixel that fixes the base distance at exactly
10.000 m) must have a face of 35 pixels, rows 0-34; the check allows +/-1 for the pixel-centre convention and the
code reads 36.

## The hue: the reading was mixing, not the paint

`o240_bank_hue_deg` ran **106.2 / 126.5 / 148.8 deg** over three seeds while the grass under it held 223.6-229.2:
a 42-deg scatter on the BANK side only, so the control was fine and the bank reading was not. The cause is
mixing. The gas card is alpha-blended, so most of the blob's area is faint fringe - mostly ground showing
through - and each seed holds a different fringe-to-core ratio, which drags the mean hue a different distance
toward the grass's 230 deg.

The fix is in the reading, not the paint (`CombatFx.Banks.cs` is untouched):

1. `HUE_CORE_PCTL = 75` and `core_of(mask, diff)`: the hue is taken over the blob's **core**, the masked pixels
   in the top quartile of plate difference, where the card's own colour dominates.
2. the core mean was still 30.8 deg apart across seeds, so the ladder of step 8 was climbed in order:

| Rung | `o240_bank_hue_deg` (s0 / s1 / s2) | Spread | Verdict |
|---|---|---|---|
| whole blob, mean (r2-3-g2) | 126.5 / 106.2 / 148.8 | 42.6 deg | the complaint |
| core 75, mean | 93.6 / 86.1 / 116.9 | 30.8 deg | still > 15 |
| core 90, mean | 93.6 / 84.3 / 131.5 | 47.2 deg | worse |
| core 95, mean | 98.5 / 83.3 / 119.7 | 36.4 deg | still > 15 |
| core 95, median | 90.6 / 84.3 / 102.4 | 18.1 deg | still > 15 |
| **core 75, median** | **88.5 / 85.7 / 100.0** | **14.3 deg** | **the rung used** |

The rung in the code is `HUE_CORE_PCTL = 75` with `median_hue`, the saturation-weighted circular MEDIAN over the
core (hues folded to signed offsets around their own mean, the weighted median of those offsets added back). A
median is outlier-proof where a mean is not: the last fringe pixels inside the core, and JPEG ringing on the
blob's edge, pull a mean and never a median. At 95 the core is only ~2900 pixels and the median gets noisy
again, which is why the 75 rung wins and the grass control steadies with it (220.8 / 223.8 / 223.0 against
344.5 / 1.2 / 221.6 at rung 95 - a plate median over 2900 dark, barely saturated pixels is meaningless).

So the three seeds now agree on one hue inside 15 deg, and `o240_hue_dist_deg` - the distance from the grass
those same core pixels show in the plate - is **120.80 / 134.53 / 138.04 deg**.

`selftest()` checks the core reading by hand (step 10): a 64x64 plate of grass (0.20, 0.25, 0.50), a 32x32 blob
whose central 16x16 is `CombatFx.GasBankNight` (0.78, 0.86, 0.22) and whose 768-pixel ring is the 50/50 blend
(0.49, 0.555, 0.36). From the HSV formula: grass 230.0 deg, core 67.5 deg, blend 80.0 deg. The core measure
reads 67.36 deg and a distance of 162.51 deg (hand: 67.5 and 162.5), while the whole blob reads 75.02 deg -
pulled toward the blend, which is what proves the restriction does the work.

## The stills are r2-3-g2's, byte for byte

`git diff --name-only 2ba27d4072..HEAD -- trench-warfare-3d/Assets/_Project/Presentation` is **empty**: nothing
drawn has changed since the stills of commit `2ba27d4072` were shot. So all 36 `gas-*` files and both manifests
are copied here unchanged rather than reshot - a reshoot of the same build would move nothing the probe reads,
and this way the two folders' numbers are comparable pixel for pixel. The harness and both passes are unchanged
(`trench-warfare-3d/Assets/_Project/Tests/Stills/GasBankStills.cs`; the commands are in
`../ability-fx-captures-r2-3-g2/README.md`).

## What is here

| File | What it is |
|---|---|
| `probe.py` | the measure. `--selftest` checks the camera maths, the texture/hue readings, `footprint_bias_m` and now the face count against hand calculations; no arguments measures the stills beside it |
| `probe.json` | the readings of this run, one value per seed, with the bars |
| `gas-t1-s*`, `gas-o240-s*`, `gas-side-s*` | three seeds, three bands, each with its `-plate` and its `.json` sidecar - r2-3-g2's files, unchanged |
| `manifest.json`, `manifest-plate.json` | one row per still: patch, sim tick, pose error, gas field before and after the call, cards drawn, draw calls |

## The bars

| Reading | Seeds (s0 / s1 / s2) | Min | Rule | Bar |
|---|---|---|---|---|
| `t1_changed_frac` | 0.2358 / 0.2599 / 0.2709 | 0.2358 | the owner's, unchanged | **>= 0.08** |
| `texture_var_drop` | 0.6337 / 0.7305 / 0.6632 | 0.6337 | the owner's, unchanged | **>= 0.40** |
| `o240_hue_dist_deg` | 134.53 / 138.04 / 120.80 | 120.80 | `min - max(10, spread)`, rounded DOWN to 0.1 | **>= 103.5** |
| `side_face_px` | 135367 / 52156 / 186565 | 52156 | `floor(0.8 * min / 1000) * 1000` | **>= 41000** |

The face bar's margin is a **share** (20 %), not a count, because face area scales with how big a patch's cloud
is - the three seeds spread 3.6x apart - and rounding DOWN to the thousand keeps the bar outside the spread at
the low end. The size-free rows (`side_face_frac`, `side_face_col_px`) sit beside it so a future run can see
whether a change moved the face or only the cloud. A bar is never lowered below the owner's, never widened to
make a run green, never set inside the spread.

The hue bar's margin is `max(10 deg, the spread)` = 17.24 deg, so it is at least one full spread wide - exactly
what the old 69.8 (5 deg under a 48-deg scatter) was missing. 103.5 is far above the owner's floor of 25.0, and
it only holds because the reading was narrowed first.

## Final numbers (`probe.json`, this build)

| Reading | Bar | s0 | s1 | s2 |
|---|---|---|---|---|
| `t1_changed_frac` | >= 0.08 | 0.2358 | 0.2599 | 0.2709 |
| `texture_var_drop` | >= 0.40 | 0.6337 | 0.7305 | 0.6632 |
| `o240_hue_dist_deg` | >= 103.5 | 134.53 | 138.04 | 120.80 |
| `side_face_px` | >= 41000 | 135367 | 52156 | 186565 |

Reported, not barred: `gas_side_height_m` (2.817 / 2.798 / 2.888 m) with `side_top_m` and `side_bias_m`,
`side_face_frac` (0.6426 / 0.6276 / 0.6148), `side_face_col_px` (109 / 111 / 143), `side_blob_px`
(210655 / 83100 / 303462), `o240_bank_hue_deg` (88.5 / 85.7 / 100.0, over the core), `o240_grass_hue_deg` (223.0 / 223.8 / 220.8), `o240_blob_hue_deg` (126.5 / 106.2 / 148.8,
the old whole-blob mean, kept so the two can be compared), `o240_core_px` (9032 / 7669 / 7147 of 36123 / 30670 /
28579 blob pixels), and `gas_t1_height_m` against `t1_footprint_bias_m` as in r2-3-g2.

All four bars, all three seeds. `probe.py` with no arguments exits 0.

## Still open

Nothing of the owner's ask. One note for him: 14.3 deg of seed-to-seed spread on the bank's core hue is under
the 15 deg the plan asked for, but it is not nothing - three patches of the same build still read 85.7 to 100.0
deg. The remainder is the bank's own per-seed card mix (how many cards overlap at the capture tick), not the
measure, and narrowing it further would mean changing the drawn look, which is concepts-first work and out of
scope here.
