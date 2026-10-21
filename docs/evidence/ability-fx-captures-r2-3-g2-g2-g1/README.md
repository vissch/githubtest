# The chlorine bank, round 3-g2-g2-g1: the tank out of the side mask

The owner (2026-10-21): "Draw one soft gas bank in the same side framing on all three seeds, with the tank out of
the mask. `side_face_px` is 52156, 135367 and 186565 and the bar is 41000, and the side-s1 still is the tank."

He is right, and the two pictures say it:

| Still | What it shows |
|---|---|
| `gas-side-s1.jpg` | a burning tank wreck filling the right half, wire pickets in front, the bank only a green carpet at the left. The still is the tank, not the bank. |
| `gas-side-s0.jpg` | a green mat over the whole lower frame - **six banks' leavings, not one**. `manifest.json` says the side block starts with `gas_cells_before` 454 / 489 / 325, because the side block is the seed's third and nothing drains between blocks. |

This folder is **part 1** of the answer: the probe's half, the tank out of the mask. The harness half - one bank
per side still and the same framing on all three seeds - is the next step and has not been done (see
"Still open"). No drawn look changed and nothing was reshot.

## Why the plate did not take the tank out by itself

The plate is supposed to be the control: whatever moved between it and the shot is the gas. In this band it is
not, for two reasons that are both in the manifests:

1. **The two passes are not on the same tick.** The plate pass skips the 8-frame wait after each block it does
   not call (`GasBankStills.Block`, the `if (!plate)` body), so it runs ahead. The new row `side_tick_skew`
   measures it from the two manifests: **5 / 4 / 4 ticks** on these stills (side gas tick 898 against plate 893;
   o240 249 against 222).
2. **The field is not empty when the side block starts.** `gas_cells_before` 454 / 489 / 325, as above.

So static scenery does **not** subtract out. Anything that merely MOVED in those ticks - a burning wreck's fire,
its smoke, infantry - lands in the mask, and on seed 1 it did.

## The two gates (`measure_side`, before `largest_blob`)

The mask is now `(|delta luma| >= LUMA_BAR) & gas_hue_gate(img) & patch_box(...)`. Everything downstream is
untouched: `face_px`, `height_of` and every other reading take the new blob exactly as they took the old one.

**The patch box** (`patch_box`, built on the new `project`) is the half that cannot be argued with. The bank is
at a known place in the world - `manifest.json` carries the patch `x`, `z` and its `ground_y` - and the camera
pose is in the sidecar. 24 points of the ring of radius `PATCH_R_M` = 14 m about the patch centre, at the ground
and `PATCH_H_M` = 10 m up, are projected and the pixel bbox clamped to the frame. A pixel outside that box is
outside the bank's own column of air, so it cannot be the bank however much it moved. 14 m is `Open()`'s 10 m
clear radius plus 4 m for wind drift; the box covers 700291 px of the 921600-px frame, so it is a loose gate -
it is there to be unarguable, not to be tight.

**The hue band** carries the rest: a wreck hull is blue-grey at about 215 deg, its fire orange at
60*((0.45-0.10)/0.90) = 23.3 deg, and the grass they stand on reads 220.8-223.8 deg (`o240_grass_hue_deg`). The
band is 40-160 deg (`GAS_HUE_DEG` 100 +/- `GAS_HUE_TOL` 60), which clears the fire by 16.7 deg and the grass by
61 deg, and holds both the paint (`CombatFx.GasBankNight` (0.78, 0.86, 0.22) = 67.5 deg) and a 50/50 blend of it
with that grass (80.0 deg), so the bank's alpha fringe stays in.

**Why the band is not centred on the paint.** The plan asked for 67.5 +/- 60 deg. That is wrong twice: it keeps
the fire (23.3 deg is only 44.2 deg from 67.5, not the 60-plus the plan assumed), and it cuts most of the drawn
bank - run with it, `side_face_px` came back **0 / 587 / 104126**. On screen the card is alpha-blended over
grass and tinted by the night lighting, so the pixels it changed read 82-160 deg (5th-95th percentile of the
side mask, all three seeds; the O240 core reads 85.7-100.0). Reaching those from 67.5 would need +/-92 deg,
which lets the fire back in. So the band is set by what it must separate, not by the paint. If it ever has to be
widened, widen `PATCH_R_M` instead: past 77 deg the fire is back.

**What each gate removed** is reported, not trusted: `side_mask_px` (ungated) 261291 / 133058 / 321538,
`side_box_px` 700291, `side_cut_frac` (the share the two gates took) **0.2321 / 0.4849 / 0.2949**. Seed 1 - the
tank still - loses nearly half its mask, and its blob shrinks from 83100 to 56886 px, from columns 0-546 and
rows 276-719 to columns 45-487 and rows 289-586: the wreck's half of the frame is gone.

## The bars

| Reading | Seeds (s0 / s1 / s2) | Min | Rule | Bar |
|---|---|---|---|---|
| `t1_changed_frac` | 0.2358 / 0.2599 / 0.2709 | 0.2358 | the owner's, unchanged | **>= 0.08** |
| `texture_var_drop` | 0.6337 / 0.7305 / 0.6632 | 0.6337 | the owner's, unchanged | **>= 0.40** |
| `o240_hue_dist_deg` | 134.53 / 138.04 / 120.80 | 120.80 | r2-3-g2-g2's, unchanged | **>= 103.5** |
| `side_face_px` | 120189 / 31917 / 131151 | 31917 | `floor(0.8 * min / 1000) * 1000` | **>= 25000** |

The face rule is r2-3-g2-g2's, unchanged; only its input changed, because the reading changed. The face bar is
therefore **lower** than the 41000 the ungated reading supported, and it has to be: 41000 was partly paid for by
pixels that were never the bank. The margin is the same 20 % share of a strictly smaller, honest reading, and it
sits outside the spread of these three seeds at the low end. A bar is never widened to make a run green.

The gates also change which seed is smallest in kind, not in rank: ungated the face ran 135367 / 52156 / 186565
(s1 smallest because its bank really is a carpet at the frame edge); gated it runs 120189 / 31917 / 131151, s1
still smallest and now by its bank alone.

## Final numbers (`probe.json`, this build)

| Reading | Bar | s0 | s1 | s2 |
|---|---|---|---|---|
| `t1_changed_frac` | >= 0.08 | 0.2358 | 0.2599 | 0.2709 |
| `texture_var_drop` | >= 0.40 | 0.6337 | 0.7305 | 0.6632 |
| `o240_hue_dist_deg` | >= 103.5 | 134.53 | 138.04 | 120.80 |
| `side_face_px` | >= 25000 | 120189 | 31917 | 131151 |

Reported, not barred: `side_cut_frac` (0.2321 / 0.4849 / 0.2949), `side_mask_px` (261291 / 133058 / 321538),
`side_box_px` (700291), `side_tick_skew` (5 / 4 / 4), `side_face_frac` (0.6339 / 0.5611 / 0.5914),
`side_face_col_px` (107 / 63 / 112), `side_blob_px` (189599 / 56886 / 221763), `gas_side_height_m`
(2.816 / 2.299 / 2.686 m) with `side_top_m` and `side_bias_m`, `o240_bank_hue_deg` (88.5 / 85.7 / 100.0 over the
core) against `o240_grass_hue_deg` (223.0 / 223.8 / 220.8), `o240_blob_hue_deg` (126.5 / 106.2 / 148.8, the old
whole-blob mean), `o240_core_px` (9032 / 7669 / 7147), and `gas_t1_height_m` (7.233 / 7.146 / 5.975) against
`t1_footprint_bias_m` (3.992 / 4.159 / 3.899).

All four bars, all three seeds. `probe.py` with no arguments exits 0.

## The hand calculations (`--selftest`)

r2-2's and r2-3-g2-g2's cases are unchanged. Three are added, each worked outside the code it checks, at the
same camera (0, 10, 0), pitch 45, yaw 0, ground 0:

| Case | By hand | The code reads |
|---|---|---|
| `project`, the forward map of `ray`: (0, 3.3333, 10) sits 11.310 deg above the centre ray, so `ndc_y` = tan(11.310)/tan(12.5) = 0.9022, row ((1-0.9022)/2)*720 - 0.5 = 34.7 | 0.9022, row 34.7 | 0.9021, row 34.7 |
| `gas_hue_gate` on three colours out of the HSV formula: bank (0.78, 0.86, 0.22) = 67.5 deg KEPT, grass (0.20, 0.25, 0.50) = 230.0 CUT, fire (1.00, 0.45, 0.10) = 60*(0.35/0.90) = 23.3 CUT | 67.5 / 230.0 / 23.3, the bank only | 67.4 / 229.9 / 23.3, the bank only |
| `patch_box`, patch centre (0, 0, 10), r = 2, h = 2: the top row is the lifted far ring point (0, 2, 12), which lies on case (a)'s ray, row 34.7; the bottom is the near ground point (0, 0, 8) at atan(10/8) = 51.34 deg, 6.34 deg below the ray, `ndc_y` = -0.5013, row 540.0 | rows 34..540 | rows 34..540 |

`project` is also checked to return None for a point behind the lens. **Rows only** in the third case: the
columns turn on the aspect and on where the ring's widest point falls, and are not hand-checked.

## The stills are r2-3-g2's, byte for byte

`git diff --name-only 2ba27d4072..HEAD -- trench-warfare-3d/Assets/_Project/Presentation` is **empty**: nothing
drawn has changed since the stills of commit `2ba27d4072` were shot. So all 36 `gas-*` files and both manifests
are copied here unchanged rather than reshot, as `-g2-g2` did - a reshoot of the same build would move nothing
the probe reads, and this way the folders' numbers are comparable pixel for pixel. The harness and both passes
are unchanged (`trench-warfare-3d/Assets/_Project/Tests/Stills/GasBankStills.cs`; the commands are in
`../ability-fx-captures-r2-3-g2/README.md`).

## Still open

The owner asked for three things. **"With the tank out of the mask" is done** and measured
(`side_cut_frac`). The other two are harness work, not probe work, and are **not done here**:

- **"One soft gas bank", not six banks' leavings.** The side block must start on an empty field: a drain before
  it, so that `gas_cells_before` reads 0 / 0 / 0, which means a `DrainFrames` block and a reshoot of both
  passes (about 12k frames each, +1500 frames a seed a pass).
- **"The same side framing on all three seeds."** The three side stills are framed by whatever patch
  `ClearView` found, so s0 is a carpet at the frame edge and s2 fills it. The framing has to be fixed in
  `GasBankStills.cs`.

Both need a Unity capture run, and both change `manifest.json`, so the bars above will be re-derived from the
reshot stills with the same rules. Until then the numbers here measure the bank of commit `2ba27d4072` with the
tank taken out, which is exactly what they claim.

## What is here

| File | What it is |
|---|---|
| `probe.py` | the measure. `--selftest` checks the camera maths, the texture/hue readings, `footprint_bias_m`, the face count and now `project`, `gas_hue_gate` and `patch_box` against hand calculations; no arguments measures the stills beside it |
| `probe.json` | the readings of this run, one value per seed, with the bars |
| `gas-t1-s*`, `gas-o240-s*`, `gas-side-s*` | three seeds, three bands, each with its `-plate` and its `.json` sidecar - r2-3-g2's files, unchanged |
| `manifest.json`, `manifest-plate.json` | one row per still: patch, sim tick, pose error, gas field before and after the call, cards drawn, draw calls |
