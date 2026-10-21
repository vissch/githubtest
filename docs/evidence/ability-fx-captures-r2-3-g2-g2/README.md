# The chlorine bank, round 3-g2-g2: the upright face, in pixels

The owner (2026-10-21): "`gas_side_height_m` stays inside 2.798 to 2.888 while `side_blob_px` runs 83100, 210655
and 303462, because the number is the silhouette top minus a 10 m footprint bias. `o240_hue_dist_deg` is 74.81,
100.89 and 123.02, and the bar of 69.8 sits 5 degrees under that scatter. Measure the upright face in pixels, and
make the three seeds agree on one hue, with both bars outside the spread of three captures of the current build."

This folder answers the first half: **the face measure**. The hue half (one hue the three seeds agree on) is not
done here - see "Still open" at the bottom.

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
| `o240_hue_dist_deg` | 100.89 / 123.02 / 74.81 | 74.81 | r2-3-g2's `min - 5 deg` floor | **>= 69.8** |
| `side_face_px` | 135367 / 52156 / 186565 | 52156 | `floor(0.8 * min / 1000) * 1000` | **>= 41000** |

The face bar's margin is a **share** (20 %), not a count, because face area scales with how big a patch's cloud
is - the three seeds spread 3.6x apart - and rounding DOWN to the thousand keeps the bar outside the spread at
the low end. The size-free rows (`side_face_frac`, `side_face_col_px`) sit beside it so a future run can see
whether a change moved the face or only the cloud. A bar is never lowered below the owner's, never widened to
make a run green, never set inside the spread.

## Final numbers (`probe.json`, this build)

| Reading | Bar | s0 | s1 | s2 |
|---|---|---|---|---|
| `t1_changed_frac` | >= 0.08 | 0.2358 | 0.2599 | 0.2709 |
| `texture_var_drop` | >= 0.40 | 0.6337 | 0.7305 | 0.6632 |
| `o240_hue_dist_deg` | >= 69.8 | 100.89 | 123.02 | 74.81 |
| `side_face_px` | >= 41000 | 135367 | 52156 | 186565 |

Reported, not barred: `gas_side_height_m` (2.817 / 2.798 / 2.888 m) with `side_top_m` and `side_bias_m`,
`side_face_frac` (0.6426 / 0.6276 / 0.6148), `side_face_col_px` (109 / 111 / 143), `side_blob_px`
(210655 / 83100 / 303462), and `gas_t1_height_m` against `t1_footprint_bias_m` as in r2-3-g2.

All four bars, all three seeds. `probe.py` with no arguments exits 0.

## Still open

The hue half of the owner's ask - the three seeds agreeing on ONE hue, so the bar can sit outside a narrow
spread instead of 5 deg under a 48-deg scatter - is **not** done here. `o240_hue_dist_deg` still runs
74.81-123.02 with the bar at r2-3-g2's 69.8.
