# The chlorine bank, round 3-g2-g2-g1: one bank, one tick, the tank out of the mask

The owner (2026-10-21): "Draw one soft gas bank in the same side framing on all three seeds, with the tank out of
the mask. `side_face_px` is 52156, 135367 and 186565 and the bar is 41000, and the side-s1 still is the tank."

He was right on all three counts, and the answer has two halves. The probe's half (the gates below) took the tank
out of the reading. The harness's half - **this capture** - took the second bank and the second tick out of the
picture itself. The stills here are **reshot on this build**, not copied.

## What changed in the harness (`GasBankStills.cs`)

| Change | Why | What it is worth |
|---|---|---|
| **Frame parity.** The 8-frame wait after a call now runs in BOTH passes; it used to sit inside the `if (!plate)` body, so the plate pass skipped 8 frames a call. | A plate from another sim tick subtracts nothing static: every man, flame and wreck that moved in between is in the mask. | `side_tick_skew` **5 / 4 / 4 -> 0 / 0 / 0**. Every still here is on its plate's own tick, o240 and t1 included. |
| **The side band goes first** (side, o240, t1 instead of o240, t1, side). | `DrainFrames` has just run before a seed's first block, so the field is empty. The side band was the seed's third and carried the two earlier banks' leavings. | `gas_cells_before` on the side rows **454 / 489 / 325 -> 0 / 0 / 0**. One bank, and one only. T1 stays last, so o240 still never holds t1's line of calls. |
| **`ClearView`**, beside `Open()`: the ground line from the side lens's own stand (41.3 m out on bearing 130) to the patch, sampled every 2 m, with no Trench / Link / Wire / Bunker / Blocked cell and no prop within half a corridor width. Widths 8, 6, 4 m. | A picket standing between lens and bank is in front of it on screen, where no probe gate can tell it from the bank's own face. | **It does not bite on this map**, and the log says so: `71 open patches, 71 of them kept at view width 0 m (0 = no corridor check, none of the widths yielded 3 seeds); seeds 48 m apart`. See "What is still not framed the same". |

Both passes, same build, on the tip of this lane:

```bash
TW_STILLS_DIR=<dir> Unity.exe -batchmode -projectPath trench-warfare-3d -runTests -testPlatform EditMode \
  -testFilter GasBankStills -testResults Logs/g.xml -logFile Logs/g.log      # the gas pass
TW_GAS_PLATE=1 TW_STILLS_DIR=<dir> ...                                       # the plate pass, same script, no gas
python docs/evidence/ability-fx-captures-r2-3-g2-g2-g1/probe.py --pack <dir>
```

## Why the plate did not take the tank out by itself

The plate is the control: whatever moved between it and the shot is the gas. In the old capture it was not, for
two reasons that were both in the manifests - the tick skew and the field that was not empty. Both are fixed
above, and the probe now **refuses** a skewed pair outright (`side_tick_bad`, `SIDE_SKEW_TICKS` = 2): the whole
side mask is a plate subtraction, so a plate from another tick is no control and its bar would mean nothing. The
gates cannot repair that; they only keep a thing that is drawn where the bank is not out of the reading.

## The two gates (`measure_side`, before `largest_blob`)

The mask is `(|delta luma| >= LUMA_BAR) & gas_hue_gate(img) & patch_box(...)`. Everything downstream is
untouched: `face_px`, `height_of` and every other reading take the blob exactly as they took the old one.

**The patch box** (`patch_box`, on `project`) is the half that cannot be argued with. The bank is at a known
place in the world - `manifest.json` carries the patch `x`, `z` and its `ground_y` - and the camera pose is in
the sidecar. 24 points of the ring of radius `PATCH_R_M` = 14 m about the patch centre, at the ground and
`PATCH_H_M` = 10 m up, are projected and the pixel bbox clamped to the frame. A pixel outside that box is outside
the bank's own column of air, so it cannot be the bank however much it moved. 14 m is `Open()`'s 10 m clear
radius plus 4 m for wind drift; the box covers 700291 px of the 921600-px frame, so it is a loose gate - it is
there to be unarguable, not to be tight.

**The hue band** carries the rest: a wreck hull is blue-grey at about 215 deg, its fire orange at
60*((0.45-0.10)/0.90) = 23.3 deg, and the grass they stand on reads 221.3-223.5 deg (`o240_grass_hue_deg`). The
band is 40-160 deg (`GAS_HUE_DEG` 100 +/- `GAS_HUE_TOL` 60), which clears the fire by 16.7 deg and the grass by
61 deg, and holds both the paint (`CombatFx.GasBankNight` (0.78, 0.86, 0.22) = 67.5 deg) and a 50/50 blend of it
with that grass (80.0 deg), so the bank's alpha fringe stays in.

**Why the band is not centred on the paint.** The plan asked for 67.5 +/- 60 deg. That is wrong twice: it keeps
the fire (23.3 deg is only 44.2 deg from 67.5, not the 60-plus the plan assumed), and it cuts most of the drawn
bank - run with it, `side_face_px` came back **0 / 587 / 104126**. On screen the card is alpha-blended over grass
and tinted by the night lighting, so the pixels it changed read 82-160 deg. Reaching those from 67.5 would need
+/-92 deg, which lets the fire back in. So the band is set by what it must separate, not by the paint. If it ever
has to be widened, widen `PATCH_R_M` instead: past 77 deg the fire is back.

**What each gate removed** is reported, not trusted: `side_mask_px` (ungated) 189398 / 139794 / 294688,
`side_box_px` 700291, `side_cut_frac` **0.2656 / 0.4141 / 0.2985**. Seed 1 still loses the largest share: its
wreck is still in the frame (below), and this is the measurement that keeps it out of the number.

## The bars, re-derived from this capture

| Reading | Seeds (s0 / s1 / s2) | Min | Rule | Bar |
|---|---|---|---|---|
| `t1_changed_frac` | 0.2261 / 0.2249 / 0.2754 | 0.2249 | the owner's, unchanged | **>= 0.08** |
| `texture_var_drop` | 0.5638 / 0.7428 / 0.6094 | 0.5638 | the owner's, unchanged | **>= 0.40** |
| `o240_hue_dist_deg` | 133.92 / 137.67 / 122.85 | 122.85 | `min - max(10.0, max - min)`, rounded down | **>= 108.0** |
| `side_face_px` | 70304 / 37666 / 114148 | 37666 | `floor(0.8 * min / 1000) * 1000` | **>= 30000** |

Both rules are r2-3-g2-g2's, unchanged; only their input changed, because both the reading and the capture
changed. Both bars moved **up** (103.5 -> 108.0, 25000 -> 30000), which is the only direction a bar ever moves
here, and each sits outside the spread of these three seeds at the low end. The face bar is still lower than the
41000 the owner quoted, and it has to be: 41000 was taken from an **ungated** reading that was partly paid for by
pixels which were never the bank - on seed 1, a burning wreck's own.

## Final numbers (`probe.json`, this build)

| Reading | Bar | s0 | s1 | s2 |
|---|---|---|---|---|
| `t1_changed_frac` | >= 0.08 | 0.2261 | 0.2249 | 0.2754 |
| `texture_var_drop` | >= 0.40 | 0.5638 | 0.7428 | 0.6094 |
| `o240_hue_dist_deg` | >= 108.0 | 133.92 | 137.67 | 122.85 |
| `side_face_px` | >= 30000 | 70304 | 37666 | 114148 |

Reported, not barred: `side_tick_skew` (**0 / 0 / 0**), `side_cut_frac` (0.2656 / 0.4141 / 0.2985),
`side_mask_px` (189398 / 139794 / 294688), `side_box_px` (700291), `side_face_frac`
(0.5612 / 0.5545 / 0.5662), `side_face_col_px` (51 / 97 / 100), `side_blob_px` (125271 / 67924 / 201617),
`gas_side_height_m` (1.648 / 2.329 / 2.419 m) with `side_top_m` and `side_bias_m`, `o240_bank_hue_deg`
(87.7 / 85.9 / 98.5 over the core) against `o240_grass_hue_deg` (221.6 / 223.5 / 221.3), `o240_blob_hue_deg`
(111.2 / 101.1 / 121.0, the old whole-blob mean), `o240_core_px` (7137 / 6941 / 6107), and `gas_t1_height_m`
(3.881 / 6.047 / 5.518) against `t1_footprint_bias_m` (3.562 / 4.175 / 3.896). Pose error is 0.0000 m on all
eighteen stills.

All four bars, all three seeds. `probe.py` with no arguments exits 0, and so does `--selftest`.

## What is still not framed the same

`gas-side-s1.jpg` **still holds a burning tank wreck** in the right half, and the three side stills still differ
in how much of the frame the bank fills. Two reasons, and both are in the evidence rather than argued away:

- **`ClearView` cannot bite on this map.** Of 71 open patches, only **2** have a clear 41.3 m ground line even at
  a 4 m corridor - a line that long across a trench map nearly always crosses a trench, a link or a wire belt -
  and those 2 stand too close together to be two seeds. Three seeds is the bar; one seed is no bar at all. So the
  width ladder ends on a last rung of 0 m (no corridor check), exactly as `SeedSpacingsM` steps down, and the rung
  that was used is printed with the seeds. Whether the corridor is clear is therefore **stated**, not assumed.
- **A wreck is dynamic, the search is static.** `Open()` and `ClearView` both read the map; a tank that burns out
  at that spot during the match cannot be searched for. The probe's two gates are what keep it out of the number
  (`side_cut_frac` 0.4141 on that seed), and they are honest in a way the framing is not: a pixel of the bank
  passes them by construction.

The bank itself is one bank on every side still now (`gas_cells_before` 0 / 0 / 0) and every still is on its
plate's tick, which is what the reading rests on. The drawn look is untouched: changing it is concepts-first work
(`CombatFx.Banks.cs`), and the owner has not asked for it.

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

## What is here

| File | What it is |
|---|---|
| `probe.py` | the measure. `--selftest` checks the camera maths, the texture/hue readings, `footprint_bias_m`, the face count, `project`, `gas_hue_gate` and `patch_box` against hand calculations; no arguments measures the stills beside it, and refuses a side still more than `SIDE_SKEW_TICKS` from its plate |
| `probe.json` | the readings of this run, one value per seed, with the bars |
| `gas-t1-s*`, `gas-o240-s*`, `gas-side-s*` | three seeds, three bands, each with its `-plate` (same pose, **same sim tick**) and its `.json` sidecar |
| `manifest.json`, `manifest-plate.json` | one row per still: patch, sim tick, pose error, gas field before and after the call, cards drawn, draw calls |
