# ability-fx-captures-r2-2-g2-g2-g2-g1-g1-g2-g1-g1: a standing overview cloud, 2026-10-22

`python probe.py` exits 0: every bar `OK`, including the two new ones, `o240_rise_floor` and `o240_base_ceiling`.
Three runs at three frames (`TW_GAS_WAIT_FRAMES` 200 / 215 / 230) are in `run1/`, `run2/` and `run3/`.

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
- **T1 is untouched:** at `TW_GAS_WAIT_FRAMES` 200 (`run1/`) it reads `green_low_share` 0.3462,
  `green_height_frac` 0.0950, `green_rise_m` 2.575, `green_area_frac` 0.01359 - digit for digit what `-g2-g1`
  published at the same wait. Runs 2 and 3 sit at other waits, so they move only in the fourth digit
  (0.3445/0.3450, 2.563/2.566), far inside their bars.
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

## The three runs of this build

One gas pass per run, all three against the one no-gas plate pass (O240 plate frame 301, T1 plate frame 796).

| | run1 (wait 200) | run2 (wait 215) | run3 (wait 230) | spread |
|---|---|---|---|---|
| T1 / O240 frame | 804 / 341 | 819 / 356 | 834 / 371 | three different frames |
| `o240_rise_m` | 8.176 | 7.768 | 7.752 | 0.424 |
| `o240_base_m` | 0.000 | 0.000 | 0.000 | 0.000 |
| `o240_blob_max_px` | 2689 | 2704 | 2696 | 15 |
| `o240_cloud_count` | 12 | 11 | 10 | 2 |
| `o240_cloud_share` | 0.9532 | 0.9582 | 0.9691 | 0.0159 |
| `o240_green_area_frac` | 0.0182 | 0.0184 | 0.0183 | 0.0002 |
| `green_rise_m` (T1) | 2.575 | 2.563 | 2.566 | 0.012 |
| `pose_error_max` | 0.000 | 0.000 | 0.000 | 0.000 |

## The bars

Every value is a capture of this build; every margin is bigger than the runs' own spread. The FAIL side is commit
`093b0be1b4`'s own committed `run1..run3` - the puddle the owner complained about - measured with **this** probe in
a scratch copy outside the repo (`probe.py` + those three runs, `--calibrate`).

| bar | cmp | value | runs | spread | margin | why |
|---|---|---|---|---|---|---|
| `o240_rise_floor` | ge | **7.0** | 8.176 / 7.768 / 7.752 | 0.424 | 0.752 | **new, the point of this unit.** The overview cloud has a top. The puddle reads 3.398 / 2.985 / 3.054 the same way and fails by over 4 m |
| `o240_base_ceiling` | le | **1.0** | 0.000 / 0.000 / 0.000 | 0.000 | 1.0 | **new.** The base sits on the mud, it does not float. A guard, not a discriminator: the puddle also reads 0.000, because what was wrong with it was the missing top |
| `o240_blob_floor` | ge | 1000 | 2689 / 2704 / 2696 | 15 | 1689 | the owner's own number, carried. 2.6x the puddle's 1039 / 1035 / 1060; the speck build before it read 119 / 117 / 116 |
| `o240_cloud_count` | ge | 3 | 12 / 11 / 10 | 2 | 7 | clouds exist at all: a blanket (one or two) fails it |
| `o240_cloud_ceiling` | lt | 15 | 12 / 11 / 10 | 2 | 3 | the owner's "under 15". The old 21 / 15 / 20 blanket build fails it |
| `o240_cloud_share` | ge | 0.85 | 0.9532 / 0.9582 / 0.9691 | 0.0159 | 0.1032 | carried. Nearly all the overview green is inside a counted cloud; the speck build's 0.4777 / 0.4413 / 0.3676 fails it |
| `o240_green_area_frac` | ge | 0.0060 | 0.0182 / 0.0184 / 0.0183 | 0.0002 | 0.0122 | carried unchanged; the standing cloud covers 2.5x what the puddle did |
| `green_low_share` | ge | 0.30 | 0.3462 / 0.3445 / 0.3450 | 0.0017 | 0.0445 | owner-decided, carried (close camera, untouched) |
| `green_height_frac` | le | 0.15 | 0.0950 / 0.0946 / 0.0947 | 0.0004 | 0.0550 | owner-decided ceiling, carried |
| `green_height_floor` | ge | 0.08 | 0.0950 / 0.0946 / 0.0947 | 0.0004 | 0.0146 | carried; still over `-g1`'s sheet range 0.0741-0.0768, which fails it |
| `green_rise_m` | ge | 2.4 | 2.575 / 2.563 / 2.566 | 0.012 | 0.163 | carried |
| `green_area_frac` | ge | 0.0135 | 0.01359 / 0.01359 / 0.01358 | 0.00001 | 0.00008 | carried |
| `pose_error_max` | lt | 0.50 | 0.000 / 0.000 / 0.000 | 0.000 | 0.50 | `GasBankStills.PoseBar` |

## What is here

- `probe.py` - `-g2-g1`'s, with the shared `column_profile`, `o240_rise_m`, `o240_base_m` and their two bars.
- `run1/`, `run2/`, `run3/` - the three captures, `TW_GAS_WAIT_FRAMES` 200 / 215 / 230, `pose_error_m` 0. Each
  holds its own two gas stills with sidecars and `manifest.json`, plus the **one shared plate** pass's two stills
  and `manifest-plate.json` (one plate for three gas runs: only the gas frames have to differ).
- `cards/` - **carried from `-g2-g1`, not reshot.** `GasBankCards` hand-packs its own cards at a fixed
  `CardWidth` and never reads `GasCloudStand`, so this build shoots the same images. They are never barred.

## The capture recipe

With this checkout's editor closed and the project free (`python Tools/editor_lock.py status`), the plate pass
once and a gas pass per run, never two batch Unity instances against this checkout at a time:

```bash
# the plate, once (TW_GAS_PLATE=1), then a gas pass per run at its own wait
TW_STILLS_DIR=%TEMP%/tw-gas-run2 TW_GAS_WAIT_FRAMES=215 \
python Tools/pipeline/run_detached.py start gas-run2 --timeout 2400 -- \
  "C:/Program Files/Unity/Hub/Editor/6000.0.50f1/Editor/Unity.exe" -batchmode \
  -projectPath <checkout>/trench-warfare-3d -runTests -testPlatform EditMode \
  -testFilter GasBankStills -testResults <dir>/stills.xml -logFile <dir>/stills.log
# then pack the gas pass, and copy run1's five plate files (two jpg, two json, manifest-plate.json) in
python docs/evidence/ability-fx-captures-r2-2-g2-g2-g2-g1-g1-g2-g1-g1/probe.py --pack <dir> --into run2
```

A GPU that is busy (other checkouts' editors) fails a pass with `d3d11: failed to create ... [8007000e]` and a
red `GasBankStills`: it leaves no PNGs, and the pass is simply run again.
