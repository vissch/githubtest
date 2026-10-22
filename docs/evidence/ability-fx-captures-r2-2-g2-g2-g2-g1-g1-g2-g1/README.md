# ability-fx-captures-r2-2-g2-g2-g2-g1-g1-g2-g1: overview clouds a player can see, and a blob-size floor, 2026-10-22

**`python probe.py` clears, exit 0.** Three fresh captures of this branch's build (`GasCloudWiden` 5 at the
overview; the close puff and `GasCloudBlock` 12 unchanged), at three different frames, every bar outside their
spread.

## The owner's complaint

> At the overview the three stills are the night map with no gas a player can see. The probe's 11, 9 and 7 clouds
> are blobs of at most 119 px (1909, 1645 and 1616 green pixels on the whole frame), and the area floor was cut to
> 0.0001 so that speck field passes. Keep the count under 15, and add a size floor above this build's biggest
> overview blob so a speck fails. Start from `lane/show/ability-fx-captures-r2-2-g2-g2-g2-g1-g1-g2`'s tip. Bar
> outside the spread of three captures of the current build.

## Why the overview was a speck field

`-g1-g1-g2` fixed the spacing and stopped there. `GasCloudBlock` 12 refills the overview cache one card per
12 x 12 block of 4 m cells, so cards stand 48 m apart - but the card's own width line still multiplied by the
local zoom `step` (2), so each card was only 11-17.6 m wide. One card per 48 m, each far too small for its block:
the gaps won, and what the probe counted as "11 clouds" were blobs of at most 119 px on a 240 m overview frame.
Nothing a player sees. The count bars could not catch it, because 11 specks count exactly like 11 clouds.

## The fix

`CombatFx.Banks.cs`, one new const:

- **`GasCloudWiden` = 5**: the overview width line reads `(step == 1 ? 1f : GasCloudWiden)` instead of
  `(step == 1 ? 1f : step)`. Same cells, same card count, same book, same `DrawPacked` call - no new draw calls
  and no new vertices, each overview card simply wider: 35.2 m at its widest against the 48 m spacing, so the
  gaps survive and the clouds fill their blocks.
- **The close branch is untouched**: `GasPuffBlock` 3, `GasPuffStand` 1.2 and the `GasPuffFirst..GasPuffLast`
  window are carried, and T1's readings prove it - `green_low_share` 0.3462 / 0.3458 / 0.3462,
  `green_height_frac` 0.0950 / 0.0933 / 0.0947, `green_rise_m` 2.575 / 2.529 / 2.568, `green_area_frac`
  0.01359 / 0.01358 / 0.01356: digit for digit what `-g1-g1-g2` published.
- **The probe**: `o240_blob_max_px` as a measure and the `o240_blob_floor` bar on it. See `probe.py`'s docstring,
  "THE PROBE'S TWO CHANGES".
- **The test** (`BurstRecipeTests.TheCloseGasPuffStands_AndTheOverviewCloudStandsOnTheMud (renamed in -g1-g1, was TheCloseGasPuffStands_AndTheMergedOutSheetStillLiesFlat)`): `widestOverviewCard`
  now reads `CombatFx.GasCloudWiden` instead of the literal `2f`, so the "cards stand further apart than the
  widest card" assert keeps meaning, and a new assert requires `GasCloudWiden > BankStep(BankMergeZoom)` - the
  speck build's own multiplier (2) fails it.

## The sweep that picked widen 5

Each row is one `GasBankStills` gas pass plus its plate pass at `TW_GAS_WAIT_FRAMES` 200 (O240 frame 341). The
rows also stand in `GasCloudWiden`'s doc comment.

| GasCloudWiden | widest card | o240_blob_max_px | o240_cloud_count | o240_cloud_share | o240_green_area_frac | verdict |
|---|---|---|---|---|---|---|
| 2 (the speck build) | 17.6 m | 119 | 11 | 0.4777 | 0.0010 | the owner's complaint: blobs nobody sees |
| 3 | 26.4 m | 295 | 16 | 0.6793 | 0.0022 | visible blobs, but every card now clears the 50 px cloud floor, so the count rises to **16** - over the owner's 15 |
| 4 | 35.2 m | 625 | 16 | 0.8170 | 0.0043 | bigger blobs, the count still 16 |
| **5** | **44.0 m** | **1039** | **13** | **0.8923** | **0.0072** | **kept**: a few neighbours touch, so 16 cards read as 13 clouds - under 15 - and the top blobs (1039 / 958 / 850 / 681 / 648 ...) stand apart evenly: thirteen clouds, not one blanket with a tail of specks |

Connected-component counts and blob sizes do not interpolate: every row is a capture, and any further change here
has to be re-measured, not guessed.

## The bars

Every bar is checked twice: all three runs on the pass side, AND the nearest run value clearing the bar by at
least the three runs' own spread (max - min).

| key | cmp | bar | the three runs | spread | margin | why |
|---|---|---|---|---|---|---|
| `green_low_share` | ge | 0.30 | 0.3462 / 0.3458 / 0.3462 | 0.0004 | 0.0458 | owner-decided, carried |
| `green_height_frac` | le | 0.15 | 0.0950 / 0.0933 / 0.0947 | 0.0017 | 0.0550 | owner-decided ceiling, carried |
| `green_height_floor` | ge | 0.08 | 0.0950 / 0.0933 / 0.0947 | 0.0017 | 0.0133 | carried, re-read; still over `-g1`'s sheet range 0.0741-0.0768 |
| `green_area_frac` | ge | 0.0135 | 0.01359 / 0.01358 / 0.01356 | 0.00003 | 0.00006 | carried, re-read: the close branch did not change |
| `green_rise_m` | ge | 2.4 | 2.575 / 2.529 / 2.568 | 0.046 | 0.129 | carried, re-read |
| `o240_blob_floor` | ge | 1000 | 1039 / 1035 / 1060 | 25 | 35 | **new, the point of this unit**: the biggest overview blob of the whole frame. The speck build reads 119 / 117 / 116 (commit `e39c8f88e1`), so it fails by an order of magnitude |
| `o240_green_area_frac` | ge | 0.0060 | 0.0072 / 0.0072 / 0.0071 | 0.0001 | 0.0011 | **re-set up**: the 0.0001 it replaces was the speck field's own coverage |
| `o240_cloud_count` | ge | 3 | 13 / 13 / 11 | 2 | 8 | the floor: clouds exist at all. One or two is a blanket from the other side |
| `o240_cloud_ceiling` | lt | 15 | 13 / 13 / 11 | 2 | 2 | the owner's rule, carried: the count stays under 15, so the old 21 / 15 / 20 blanket fails |
| `o240_cloud_share` | ge | 0.85 | 0.8923 / 0.9196 / 0.8911 | 0.0285 | 0.0411 | **re-set up**: nearly all the overview green now sits inside a counted cloud - the 50-150 px fringe specks are gone. The speck build's 0.4777 / 0.4413 / 0.3676 fails it |
| `pose_error_max` | lt | 0.50 | 0 / 0 / 0 | 0 | 0.50 | `GasBankStills.PoseBar` |

Why there is no overview height bar: the overview card's drawn band does grow with its width, and the owner's
height rules (`green_height_frac`, `green_height_floor`) are the close camera's. The test's `sheet` assert reads
the BASE card at 1:1, which `GasCloudWiden` does not touch; its comment now says so.

## What is here

- `probe.py` - `-g1-g1-g2`'s, with exactly two changes (`o240_blob_max_px`, the `o240_blob_floor` bar) plus the
  re-derived `BAR_VALUES` and a docstring for this unit.
- `run1/`, `run2/`, `run3/` - the four `GasBankStills` captures with sidecars and manifests, at
  `TW_GAS_WAIT_FRAMES` 200 / 215 / 230 - T1 frames 804 / 819 / 834, O240 frames 341 / 356 / 371.
  `pose_error_m` 0 everywhere. `run1` is the sweep's kept row, reshot as a run.
- `cards/` - **carried from `-g1-g1-g2`, not reshot.** `GasBankCards` hand-packs its own cards at a fixed
  `CardWidth` and never reads `GasCloudWiden`, so this build would shoot the same images. They are never barred.
- `probe.json` - all three runs' numbers, frames, the look report and `BAR_VALUES`.

## The capture recipe

Per run, with this checkout's editor closed and the project free (`python Tools/editor_lock.py status`), two batch
passes into the SAME directory at the SAME wait, so the plate hits the shot's frame:

```bash
# gas pass, then the same line again with TW_GAS_PLATE=1
TW_STILLS_DIR=%TEMP%/tw-gasbank-w5f2 TW_GAS_WAIT_FRAMES=215 \
python Tools/pipeline/run_detached.py start gas-w5f2 --timeout 2400 -- \
  "C:/Program Files/Unity/Hub/Editor/6000.0.50f1/Editor/Unity.exe" -batchmode \
  -projectPath <checkout>/trench-warfare-3d -runTests -testPlatform EditMode \
  -testFilter GasBankStills -testResults <out>/stills-f2.xml -logFile <out>/stills-f2.log
python docs/evidence/ability-fx-captures-r2-2-g2-g2-g2-g1-g1-g2-g1/probe.py --pack %TEMP%/tw-gasbank-w5f2 --into run2
```

Runs 1 and 3 are the same at `TW_GAS_WAIT_FRAMES` 200 and 230, into `%TEMP%/tw-gasbank-w5` and
`%TEMP%/tw-gasbank-w5f3`. Only one batch Unity instance may run against this checkout at a time (they all point
at the same `-projectPath`): the gas and plate passes, and each run, one after another, never in parallel.

`cards/` needs no launch in this unit - it is carried. Reshooting it (if the isolation cards ever change) is one
`GasBankCards` launch, no wait knob and no plate pass; the recipe is in `-g1-g1-g2/README.md`.
