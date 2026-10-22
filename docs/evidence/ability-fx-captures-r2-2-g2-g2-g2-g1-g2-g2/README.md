# ability-fx-captures-r2-2-g2-g2-g2-g1-g2-g2: the far count keeps only clouds (part 1 of 2), 2026-10-22

**Part 1 of 2.** This leg fixes the far-view fault only — the O240 cloud count — in `probe.py`. `CombatFx` is NOT
edited, so `run1/`, `run2`, `run3` are copied in from `-g1-g2/` unchanged: they are captures of the SAME build
`-g1-g2` shot, not fresh ones. The close-puff fault (the flat sheet of pads) is not fixed here; part 2 reshoots
three fresh captures against a `CombatFx` change and bars the puff's stand-off.

## The owner's complaint

> At zoom 30 the close gas is a sheet of flat green pads, and the far view counts specks as clouds. Reshoot three
> fresh captures so the close puff stands off the mud and the far count keeps only clouds a player can see. Start
> from `lane/show/ability-fx-captures-r2-2-g2-g2-g2-g1-g2`'s tip. Bar outside the spread of three captures of the
> current build.

## What is here

- `probe.py` — carried from `-g1-g2` with one change: `CLOUD_MIN_PX` 50 -> 200. 50 is a 7 px speck in the
  1280x720 = 921,600 px frame; at 50 the probe counted 20 / 15 / 20 components as clouds on captures whose own
  manifest (`calls_taken`) says 5. 200 px (0.022% of the frame, ~14 px across) is the smallest green patch a
  player actually reads at O240; on these SAME carried captures' `o240_blob_top` (unchanged) it keeps exactly 4
  clouds in every run (run1 4274/1242/1058/620 then 145, run2 4537/2324/375/269 then 157, run3
  4858/2312/332/209 then 166). `BARS` also gains `o240_cloud_ceiling` on the same measure, so a fragmented wash
  cannot pass a floor-only bar that never refuses "too many" — the same floor/ceiling pair trick `-g1` used for
  height. The camera maths, the green mask, `largest_blob`, T1's bars, `--pack`, `--calibrate`, `--selftest` and
  the spread check are otherwise untouched.
- `run1/`, `run2/`, `run3/` — copied from `-g1-g2` unchanged: the same four `GasBankStills` captures of the
  unchanged build (T1 frames 804 / 819 / 834, O240 frames 341 / 356 / 371). **These are still captures of the
  current build, not fresh ones — the close-puff flat-pad fault named in the owner's complaint is NOT fixed by
  this leg.** Part 2 reshoots.
- No `cards/` — out of scope for this lane (a different unit's goal per the plan).

## The bars

Carried from `-g1-g2` unchanged except the four below, re-measured under `CLOUD_MIN_PX` 200 (confirmed with
`--calibrate`):

| bar | old | new | runs | spread | margin |
|---|---|---|---|---|---|
| `o240_cloud_count` (`ge`) | 10 | **3** | 4 / 4 / 4 | 0 | 1 |
| `o240_cloud_ceiling` (`le`, new) | — | **6** | 4 / 4 / 4 | 0 | 2 |
| `o240_cloud_share` (`ge`) | 0.85 | **0.65** | 0.727 / 0.765 / 0.752 | 0.038 | 0.077 |
| `o240_green_area_frac` (`ge`) | 0.008 | **0.006** | 0.0078 / 0.0081 / 0.0084 | 0.0006 | 0.0018 |

Every other bar (T1's five, `pose_error_max`) is unchanged from `-g1-g2` and still clears on these captures.

## Done when

`python probe.py` exits 0 (confirmed this leg: every bar `OK`, `o240_cloud_count` 4 in each run).
