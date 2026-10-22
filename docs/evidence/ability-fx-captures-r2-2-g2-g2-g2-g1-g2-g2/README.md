# ability-fx-captures-r2-2-g2-g2-g2-g1-g2-g2: the close puff stands off the mud, the far count keeps only clouds, 2026-10-22

Both parts done. Part 1 fixed the far-view fault (the O240 cloud count) in `probe.py` only, against captures
carried unchanged from `-g1-g2`. Part 2 (below) fixes the close-puff fault with a `CombatFx` change and reshoots
three fresh captures; `run1/`, `run2/`, `run3/` are now those fresh captures, not the carried ones from part 1.

## The owner's complaint

> At zoom 30 the close gas is a sheet of flat green pads, and the far view counts specks as clouds. Reshoot three
> fresh captures so the close puff stands off the mud and the far count keeps only clouds a player can see. Start
> from `lane/show/ability-fx-captures-r2-2-g2-g2-g2-g1-g2`'s tip. Bar outside the spread of three captures of the
> current build.

## Part 1: the far count keeps only clouds

`probe.py` carried from `-g1-g2` with one change: `CLOUD_MIN_PX` 50 -> 200. 50 is a 7 px speck in the
1280x720 = 921,600 px frame; at 50 the probe counted 20 / 15 / 20 components as clouds on captures whose own
manifest (`calls_taken`) says 5. 200 px (0.022% of the frame, ~14 px across) is the smallest green patch a
player actually reads at O240; on the captures carried from `-g1-g2` (`o240_blob_top` unchanged) it keeps exactly
4 clouds in every run (run1 4274/1242/1058/620 then 145, run2 4537/2324/375/269 then 157, run3
4858/2312/332/209 then 166). `BARS` also gained `o240_cloud_ceiling` on the same measure, so a fragmented wash
cannot pass a floor-only bar that never refuses "too many" — the same floor/ceiling pair trick `-g1` used for
height. The camera maths, the green mask, `largest_blob`, T1's bars, `--pack`, `--calibrate`, `--selftest` and
the spread check were otherwise untouched by part 1.

## Part 2: the close puff stands off the mud

`CombatFx.Banks.cs`, `DrawGasBank`: the close card (`step == 1`) was anchored at `b.Ground - 0.2f`, so a 5-7 m
wide card 1.6-2.2 m tall sat sunk in the mud and read as a flat pad. Added `CombatFx.GasPuffLift` and anchor the
close card at `b.Ground + GasPuffLift`. The merged-out line (`- 0.35f`, `step != 1`) is untouched: outside
`BankMergeZoom` the bank stays the squashed blanket. `GasPuffStand` stays 0.32.

`BurstRecipeTests.TheCloseGasPuffStands_AndTheMergedOutSheetStillLiesFlat` gained two asserts: `GasPuffLift` is
over 0 (the puff's bottom edge is off the mud) and under 0.6 (it does not float at a man's waist).

**`GasPuffLift` landed at 0.025, not the planned 0.3.** Measured, not guessed, with single gas+plate passes at
`TW_GAS_WAIT_FRAMES` 200/215/230:

| lift | `green_low_share` (wait 230, the worst case) |
|---|---|
| 0.3 | 0.3002 — dead on the owner's floor, no room |
| 0.2 | 0.3004 — same, no room |
| 0.1 | 0.3079 |
| 0.05 | 0.3189 |
| 0.025 | 0.3237 |
| 0.01 | 0.3204 — noisier than 0.025, no better |

The measure is far more sensitive between "sunk 0.2 m" (the old anchor, `green_low_share` 0.42) and "any lift at
all" than between different lift values in 0.01-0.3: most of the climb away from the mud happens in the first
few centimetres, then the three captured frames' own variance (which cloud shape each wait samples) dominates.
0.025 was the value, of the ones tried, whose worst wait still cleared the floor with room for the three-run
spread; 0.3, this plan's prediction, did not.

## Three fresh runs

`run1/`, `run2/`, `run3/` (`TW_GAS_WAIT_FRAMES` 200 / 215 / 230) replace the carried captures from part 1. Recipe
below. T1 frames 804 / 819 / 834 (plate 796 / 811 / 826), O240 frames 341 / 356 / 371 (plate 301 / 316 / 331).

## The bars

Rebarred from the fresh captures (`--calibrate`); `green_low_share` 0.30 and `green_height_frac` 0.15 are
owner-decided and were not moved. O240's four bars (set in part 1) are unchanged: the close-puff lift only
touches the close (`step == 1`) card, not the merged-out O240 band, so their measures are carried exactly.

| bar | cmp | value | runs | spread | margin |
|---|---|---|---|---|---|
| `green_low_share` | ge | 0.30 (owner, kept) | 0.3440 / 0.3461 / 0.3237 | 0.0224 | 0.0237 |
| `green_height_frac` | le | 0.15 (owner, kept) | 0.0862 / 0.0858 / 0.0858 | 0.0004 | 0.0638 |
| `green_height_floor` | ge | 0.05 (kept) | 0.0862 / 0.0858 / 0.0858 | 0.0004 | 0.0358 |
| `green_area_frac` | ge | **0.07** (was 0.04) | 0.0803 / 0.0797 / 0.0840 | 0.0043 | 0.0097 |
| `green_rise_m` | ge | **2.5** (was 2.0) | 2.582 / 2.568 / 2.568 | 0.014 | 0.068 |
| `o240_green_area_frac` | ge | 0.006 (carried) | 0.0078 / 0.0081 / 0.0084 | 0.0006 | 0.0018 |
| `o240_cloud_count` | ge | 3 (carried) | 4 / 4 / 4 | 0 | 1 |
| `o240_cloud_ceiling` | le | 6 (carried) | 4 / 4 / 4 | 0 | 2 |
| `o240_cloud_share` | ge | 0.65 (carried) | 0.727 / 0.765 / 0.752 | 0.038 | 0.077 |
| `pose_error_max` | lt | 0.50 | 0 / 0 / 0 | 0 | 0.50 |

## The capture recipe

Per run, with this checkout's editor closed and the project free (`python Tools/editor_lock.py status`), two
batch passes into the SAME directory at the SAME wait, so the plate hits the shot's frame:

```bash
# gas pass, then the same line again with TW_GAS_PLATE=1
TW_STILLS_DIR=%TEMP%/tw-gasbank-f1 TW_GAS_WAIT_FRAMES=200 \
python Tools/pipeline/run_detached.py start gas-f1 --timeout 2400 -- \
  "C:/Program Files/Unity/Hub/Editor/6000.0.50f1/Editor/Unity.exe" -batchmode \
  -projectPath <checkout>/trench-warfare-3d -runTests -testPlatform EditMode \
  -testFilter GasBankStills -testResults <out>/stills-f1.xml -logFile <out>/stills-f1.log
# then, same dir, same wait:
TW_STILLS_DIR=%TEMP%/tw-gasbank-f1 TW_GAS_WAIT_FRAMES=200 TW_GAS_PLATE=1 \
python Tools/pipeline/run_detached.py start gas-f1-plate --timeout 2400 -- \
  "C:/Program Files/Unity/Hub/Editor/6000.0.50f1/Editor/Unity.exe" -batchmode \
  -projectPath <checkout>/trench-warfare-3d -runTests -testPlatform EditMode \
  -testFilter GasBankStills -testResults <out>/stills-f1-plate.xml -logFile <out>/stills-f1-plate.log
python probe.py --pack %TEMP%/tw-gasbank-f1 --into run1
```

Runs 2 and 3 are the same at `TW_GAS_WAIT_FRAMES` 215 and 230: three waits are three frames, and the probe
refuses two runs that photographed one frame.

## Done when

`python probe.py` exits 0 (confirmed this leg: every bar `OK`, `o240_cloud_count` 4 in each run).

## Out of scope

The `cards/` isolation set and the three card bars (`cards_gaspuff_rise_m`, `cards_gaspuff_aspect`, the
gaspuff/smoke relief ratio) that `-g1-g2`'s README calls "part 2" of its own plan — a different unit's goal.
