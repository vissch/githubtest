# The chlorine bank, round 3-g2: a side, not just a disc from above

The owner (2026-10-20): "`gas_t1_height_m` says 5.7 to 7.2 m, and the same stills are flat discs with no side a
player can see. Shoot the bank so its thickness is on screen, and put a height bar outside the spread of three
captures of the current build. The hue bar of 38 sits under seeds that already run 69 to 126, so it does not
police the look."

## Why 5.7-7.2 m was not a side

Both T1 (pitch 25, zoom 30) and O240 (pitch 25, zoom 240) look down steeply enough that ground lying further out
than the bank's own base reads as height, not footprint. `height_of` fixes a column's distance from its BOTTOM
pixel and reads every higher pixel as height at THAT distance - so ground 10 m beyond the column base, which
sits a little higher in frame purely by perspective, gets counted as the bank standing taller.

- **T1's pose:** zoom 30, fov 25, so distance = `30*tan(30deg)/tan(12.5deg)` = 78.1 m, camera 33.0 m up, 70.8 m
  out. Ground 10 m beyond the bank's base sits 2.8 deg higher in frame, which `height_of` turns into
  `70.8*tan(2.8deg)` = **3.5 m** of "height" that is only footprint depth - about half of the 5.7-7.2 m T1 claims.
- **The new side pose:** zoom 16 (just above `CloseZoom` 15, so the lens stays at `TacticalCamera.Fov` 25),
  pitch 8 (`TacticalCamera.PitchMin`, as low as `CaptureRig`'s `Pose` clamp allows). Distance 41.7 m, camera
  7.8 m up, 41.3 m out. The same 10 m of ground depth now reads as only `41.3*tan(2.05deg)` = **1.5 m**. The lens
  grazes the bank's upright body card edge-on instead of looking down on it.

`aimY` is raised to `ground + 2 m` (`CaptureRig.Shot`'s overload) because the default aim (world y = 0) at this
zoom puts the horizon out of frame on raised patches.

## The side measure

`measure_side` plate-subtracts exactly as `measure_t1` does (`LUMA_BAR`, `largest_blob`), then per column with
`>= MIN_COLUMN_PX` pixels takes the column's **TOP** pixel (the silhouette top, not T1's 45th-percentile card
pick) at the column base's distance. The median over columns is `side_top_m`. `footprint_bias_m` is what
`SIDE_FOOT_DEPTH_M` (10 m - the bank's own spread; `Open()` clears a 10 m radius) of extra ground depth reads as,
in metres, at that distance - the honest error band of any single-view height claim, worked by hand in
`selftest()`: camera 10 m up, pitch 45, base distance 10 m; ground 10 m further out (20 m) has depression
`atan(10/20)` = 26.565 deg against the base's 45 deg, a difference of 18.435 deg, so at the 10 m plane it reads
`10*tan(18.435)` = **3.3333 m**. `gas_side_height_m = max(0, side_top_m - footprint_bias_m)`: subtracting the
bias is what stops the T1 lie coming back. `t1_footprint_bias_m` is reported alongside `gas_t1_height_m` so the
table below can show how much of the 5.7-7.2 m claim was only depth.

A pose guard fails the probe (exit 1, not a silent pass) if a side sidecar's `cam_pitch` is not within 1 deg of
8, or its camera does not stand within 1.5 m of 7.8 m above the ground: the bar only means anything at the pose
it was set at.

## What is here

| File | What it is |
|---|---|
| `probe.py` | the measure. `--selftest` checks the camera maths, the texture/hue readings and `footprint_bias_m` against hand calculations; no arguments measures the stills beside it |
| `probe.json` | the readings of this run, one value per seed, with the bars |
| `gas-t1-s*`, `gas-o240-s*`, `gas-side-s*` | three seeds, three bands, each with its `-plate` (same pose, same sim tick, gas calls skipped) and its `.json` sidecar |
| `manifest.json`, `manifest-plate.json` | one row per still: patch, sim tick, pose error, gas field before and after the call, cards drawn, draw calls |

The harness is `trench-warfare-3d/Assets/_Project/Tests/Stills/GasBankStills.cs` (now three bands a seed, 18
stills a pass). Both passes, same build:

```bash
TW_STILLS_DIR=<dir> Unity.exe -batchmode -projectPath trench-warfare-3d -runTests -testPlatform EditMode \
  -testFilter GasBankStills -testResults Logs/g.xml -logFile Logs/g.log      # the gas pass
TW_GAS_PLATE=1 TW_STILLS_DIR=<dir> ...                                       # the plate pass, same script, no gas
python docs/evidence/ability-fx-captures-r2-3-g2/probe.py --pack <dir>
```

Shot here on the tip of `lane/show/ability-fx-captures-r2-3`, commit `2ba27d4072` - the look the owner has not
asked to change.

## The bars

A bar sits just outside the low end of the three seeds' spread: `bar = min(seeds) - margin`. The seeds spread
48-58 deg of hue and under 0.1 m of height apart - far wider than the floors below - so a spread-sized margin on
the hue reading would push the bar BELOW the 38.0 r2-3 already set, undoing "it does not police the look" rather
than fixing it. The floor governs instead, and the arithmetic is rounded DOWN so rounding never moves a bar
inside the spread:

| Reading | Seeds (s0 / s1 / s2) | Min | Floor | Bar (`min - floor`, rounded down) |
|---|---|---|---|---|
| `gas_side_height_m` | 2.817 / 2.798 / 2.888 | 2.798 | 0.5 m | **>= 2.2** |
| `o240_hue_dist_deg` | 100.89 / 123.02 / 74.81 | 74.81 | 5.0 deg | **>= 69.8** (was 38.0) |

Never lowered below the owner's bar (25 deg for hue; `gas_side_height_m` is new, so it has none), never widened
to make a run green, never set inside the spread. `t1_changed_frac` (>= 0.08) and `texture_var_drop` (>= 0.40)
are unchanged from r2-3 - this run clears both easily (worst 0.2358 / 0.6337).

`gas_cells_before` on a side row is the T1 block's own cloud on the same patch, by design: the side block shoots
the same seed's patch a few hundred frames after the T1 block gassed it, so a non-zero reading there is this
seed's own gas, not another's (the O240 overview shoots first and always reads 0, same as r2-3).

## Final numbers (`probe.json`, this build)

| Reading | Bar | s0 | s1 | s2 |
|---|---|---|---|---|
| `t1_changed_frac` | >= 0.08 | 0.2358 | 0.2599 | 0.2709 |
| `texture_var_drop` | >= 0.40 | 0.6337 | 0.7305 | 0.6632 |
| `o240_hue_dist_deg` | >= 69.8 | 100.89 | 123.02 | 74.81 |
| `gas_side_height_m` | >= 2.2 | 2.817 | 2.798 | 2.888 |

Reported, not barred: `gas_t1_height_m` (5.975-7.233 m) against `t1_footprint_bias_m` (the arithmetic above
worked per-seed: 3.899-4.159 m of each T1 reading is footprint depth, not height - more than half of the claim)
and `side_top_m` (4.623-5.040 m, the side band's silhouette top before `side_bias_m` (1.806-2.216 m) is
subtracted).

All four bars, all three seeds. `probe.py` with no arguments (a clean extract of this folder) exits 0.
