# ability-fx-captures-r2-2-g2-g2-g2-g1-g1-g2-g1-g1: a standing overview cloud, 2026-10-22

**Work in progress: `python probe.py` exits 1 with "no bar set yet" for `o240_rise_floor` and
`o240_base_ceiling`.** The production change, the test and the probe are in, and `run1/` is the sweep's kept row.
`run2/` and `run3/` (two more frames) and the two new bar values are the next leg's.

## The owner's complaint

> At the 240 m camera the gas is a flat lime puddle on the mud. The gas card's `rise_m` is 0, and the three
> overview stills are the same smears. Paint a standing cloud a player can read at that camera, with a top and a
> base, keep the biggest blob above 1000 px, and keep the cloud count under 15. Start from
> `lane/show/ability-fx-captures-r2-2-g2-g2-g2-g1-g1-g2-g1`'s tip. Bar outside the spread of three captures of the
> current build.

## Why the overview was flat

The overview card drew the `GasBank` book, whose drawing is a band in the bottom third of the card (ink
0.00-0.35), squashed again by `GasBankSquash` 0.55 and sunk to `Ground - 0.35`. Wide, bandy and lying on the mud:
a smear with no top. The close camera already had the fix - `GasPuff` is the smoke bank's mound (ink 0.03-0.87) in
gas green, a shape with a top and a base.

## The fix

`CombatFx.Banks.cs`, one new const:

- **`GasCloudStand` = 3.0**: the overview card now draws the same `GasPuff` book as the close card, at the same
  `GasPuffFirst..GasPuffLast` 12..16 window, anchored at `Ground - 0.2` like the close puff, and
  `GasCloudStand` x its **base** width tall (~7 m base, so 21 m of cloud). The width line is split into `baseW`
  and `width = baseW * (step == 1 ? 1f : GasCloudWiden)`, so the close card and the widened overview card keep
  exactly their old widths, and the stand does not grow with `GasCloudWiden`.
- **Unchanged:** the cells, the card count, the single `DrawPacked` call (no new draws), the close branch
  (`GasPuffBlock` 3, `GasPuffStand` 1.2), and `GasCloudWiden` 5 / `GasCloudBlock` 12. `GasBankSquash` and
  `GasBankFirst/Last` stay because `GasBankCards` still draws the `GasBank` book's isolation card with them.
- **T1 is untouched, digit for digit:** `green_low_share` 0.3462, `green_height_frac` 0.0950, `green_rise_m`
  2.575, `green_area_frac` 0.01359 at every sweep row - what `-g2-g1` published.
- **The test** (`BurstRecipeTests.TheCloseGasPuffStands_AndTheOverviewCloudStandsOnTheMud`): the old "still a
  sheet under a 1.8 m man's head" assert - the puddle itself - is replaced by the overview's own
  `7 * GasCloudStand` between 5 m (it has a top) and 30 m (it is a cloud, not a wall).
- **The probe**: `column_profile(...)` (`measure_t1`'s per-column height maths, now shared), `o240_rise_m` and
  `o240_base_m`, and the `o240_rise_floor` / `o240_base_ceiling` bars on them. See `probe.py`'s docstring.

## The sweep that picked stand 3.0

One `GasBankStills` gas pass per row at `TW_GAS_WAIT_FRAMES` 200, against one no-gas plate pass (O240 frame 341).

| GasCloudStand | cloud height | o240_rise_m | o240_blob_max_px | o240_cloud_count | o240_cloud_share | o240_green_area_frac | verdict |
|---|---|---|---|---|---|---|---|
| the smear (GasBank band, squash 0.55) | ~1.4 m of drawing | 3.398 / 2.985 / 3.054 | 1039 | 13 | 0.8923 | 0.0072 | the owner's complaint, measured on commit `093b0be1b4`'s own three runs |
| 1.2 | 8.4 m | 3.047 | 1034 | 12 | 0.8875 | 0.0071 | no higher than the smear it replaces: it could not be barred apart from it |
| 2.0 | 14 m | 5.325 | 1781 | 12 | 0.9278 | 0.0121 | a cloud with a top |
| **3.0** | **21 m** | **8.176** | **2689** | **12** | **0.9532** | **0.0182** | **kept**: the highest rise of the sweep that still holds the blob floor (2689 px) and the cloud ceiling (12, under 15), base still on the mud (`o240_base_m` 0.000) |

Connected-component counts and blob sizes do not interpolate: every row is a capture.

## What is here

- `probe.py` - `-g2-g1`'s, with the shared `column_profile`, `o240_rise_m`, `o240_base_m` and their two bars.
- `run1/` - the kept sweep row, `TW_GAS_WAIT_FRAMES` 200 (T1 frame 804, O240 frame 341), `pose_error_m` 0.
- `cards/` - **carried from `-g2-g1`, not reshot.** `GasBankCards` hand-packs its own cards at a fixed
  `CardWidth` and never reads `GasCloudStand`, so this build shoots the same images. They are never barred.

## The capture recipe

With this checkout's editor closed and the project free (`python Tools/editor_lock.py status`), the plate pass
once and a gas pass per run, never two batch Unity instances against this checkout at a time:

```bash
# the plate, once (TW_GAS_PLATE=1), then a gas pass per run at its own wait
TW_STILLS_DIR=%TEMP%/tw-gasbank-stand-s30 TW_GAS_WAIT_FRAMES=200 \
python Tools/pipeline/run_detached.py start gas-s30 --timeout 2400 -- \
  "C:/Program Files/Unity/Hub/Editor/6000.0.50f1/Editor/Unity.exe" -batchmode \
  -projectPath <checkout>/trench-warfare-3d -runTests -testPlatform EditMode \
  -testFilter GasBankStills -testResults <dir>/stills.xml -logFile <dir>/stills.log
# then copy the plate's four files + manifest-plate.json into the row's dir and pack it
python docs/evidence/ability-fx-captures-r2-2-g2-g2-g2-g1-g1-g2-g1-g1/probe.py --pack <dir> --into run1
```

A GPU that is busy (other checkouts' editors) fails a pass with `d3d11: failed to create ... [8007000e]` and a
red `GasBankStills`: it leaves no PNGs, and the pass is simply run again.
