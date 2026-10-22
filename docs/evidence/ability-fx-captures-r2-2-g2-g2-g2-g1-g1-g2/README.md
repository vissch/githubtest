# ability-fx-captures-r2-2-g2-g2-g2-g1-g1-g2: overview clouds with gaps, a count ceiling, real digits, 2026-10-22

**`python probe.py` clears, exit 0.** Three fresh captures of this branch's build (`GasCloudBlock` 12 at the
overview, `-g1-g1`'s close puff unchanged), at three different frames, every bar outside their spread.

## The owner's complaint

> The overview is still the old squash blanket, 21, 15 and 20 blobs, and the count floor of 8 sits under that
> speck field. Reshoot the overview from this build and put a cloud-count ceiling outside that spread. The close
> green area is 0.0136 three times, so the 0.01 floor is not three captures. Start from
> `lane/show/ability-fx-captures-r2-2-g2-g2-g2-g1-g1`'s tip. Bar outside the spread of three captures of the
> current build.

## Why the overview was still a blanket

`-g1-g1` fixed the close camera and said O240 "did not move, by design". It did not: the merged-out branch of
`DrawGasBank` stepped `Refresh` by the **zoom step itself (2)**, so overview cards stood 8 m apart while each one
is 11-15 m wide. They overlapped into one blanket, and the blanket's thin fringe cells each left a 50-150 px
speck. That is what the 21 / 15 / 20 "clouds" counted: `-g1-g1/probe.json`'s `o240_blob_top` is one 4275 px blob
followed by a tail of specks. A floor of 8 under a speck field is a bar the complaint can pass.

## The fix

`CombatFx.Banks.cs`:
- **`GasCloudBlock` = 12**: the merged-out draw now calls `Refresh` with block 12, so one overview card stands for
  a 12 x 12 block of cells (48 m apart at `MapData.FieldCellSize` 4 m) instead of 8 m apart. The card's own width
  still multiplies by the local zoom step, so the block widens only the **gap**. The close branch still uses
  `GasPuffBlock` 3 - nothing about the close puff changed in this unit, and T1's numbers confirm it.
- **The probe**: `green_area_frac` at five decimals, a new `o240_cloud_ceiling` bar, and the three O240 bars
  re-derived. See `probe.py`'s docstring, "THE PROBE'S THREE CHANGES".

T1 did not move at all: `green_low_share` 0.3462/0.3458/0.3462, `green_height_frac` 0.0950/0.0933/0.0947,
`green_rise_m` 2.575/2.529/2.568 - the same readings `-g1-g1` published, because only the merged-out branch
changed. Its `green_area_frac` is the same measure read to one more digit.

## What is here

- `probe.py` - `-g1-g1`'s, with exactly three changes (the five-decimal area, the `o240_cloud_ceiling` bar, the
  re-derived `BAR_VALUES`) and the docstring that explains them.
- `run1/`, `run2/`, `run3/` - the four `GasBankStills` captures with sidecars and manifests, at
  `TW_GAS_WAIT_FRAMES` 200 / 215 / 230 - T1 frames 804 / 819 / 834, O240 frames 341 / 356 / 371. `pose_error_m` 0
  everywhere. `run1` is the sweep's kept row, reshot as a run.
- `cards/` - reshot against this build. The isolation cards do not change (the overview card's own drawing is
  untouched, only how far apart the cards stand), and they are never barred.
- `probe.json` - all three runs' numbers, frames, the look report and `BAR_VALUES`.

## The sweep that picked block 12

Each row is one `GasBankStills` gas pass plus its plate pass at `TW_GAS_WAIT_FRAMES` 200 (O240 frame 341). The
rows also stand in `GasCloudBlock`'s doc comment.

| GasCloudBlock | o240_cloud_count | o240_cloud_share | o240_green_area_frac | verdict |
|---|---|---|---|---|
| 2 (the old blanket) | 21 / 15 / 20 | 0.87 | 0.0094 | one 4275 px blob plus a tail of fringe specks - the complaint |
| 3 | 34 | 0.7515 | top blob 1173 px | cards still touch: a smaller blanket |
| 4 | 29 | 0.6231 | 0.0028 | the blanket is gone, every card stands alone |
| 6 | 19 | 0.5933 | 0.0021 | |
| 8 | 17 | 0.6568 | 0.0020 | |
| 16 | 13 | 0.5888 | `o240_green_px` 2055 | the count does **not** fall with the block past 12 |
| **12** | **11** | **0.4777** | **0.0010** | **kept: the fewest clouds of the sweep, so a ceiling under 15 has room** |

Connected-component counts do not interpolate: every row is a capture, and any further change here has to be
re-measured, not guessed.

## The bars

Every bar is checked twice: all three runs on the pass side, AND the nearest run value clearing the bar by at
least the three runs' own spread (max - min).

| key | cmp | bar | the three runs | spread | margin | why |
|---|---|---|---|---|---|---|
| `green_low_share` | ge | 0.30 | 0.3462 / 0.3458 / 0.3462 | 0.0004 | 0.0458 | owner-decided, kept |
| `green_height_frac` | le | 0.15 | 0.0950 / 0.0933 / 0.0947 | 0.0017 | 0.0550 | owner-decided ceiling, kept |
| `green_height_floor` | ge | 0.08 | 0.0950 / 0.0933 / 0.0947 | 0.0017 | 0.0133 | re-derived, unmoved; still over `-g1`'s sheet range 0.0741-0.0768 |
| `green_area_frac` | ge | 0.0135 | 0.01359 / 0.01358 / 0.01356 | 0.00003 | 0.00006 | the owner's point: at five decimals the three runs differ (blob 12528 / 12511 / 12501 px), so the floor sits just outside the real spread instead of at a slack 0.01 |
| `green_rise_m` | ge | 2.4 | 2.575 / 2.529 / 2.568 | 0.046 | 0.129 | re-derived, unmoved |
| `o240_green_area_frac` | ge | 0.0001 | 0.0010 / 0.0008 / 0.0006 | 0.0004 | 0.0005 | **re-set down**: the gaps are the point, so coverage falls (0.0094 was the blanket). 0.0002 would be margin == spread, which binary floats read as a hair short |
| `o240_cloud_count` | ge | 3 | 11 / 9 / 7 | 4 | 4 | the floor: clouds exist at all. One or two clouds is a blanket from the other side |
| `o240_cloud_ceiling` | lt | 15 | 11 / 9 / 7 | 4 | 4 | **new**: the count stays under 15, so `-g1-g1`'s 21 / 15 / 20 speck field fails outright (its own 15 included) |
| `o240_cloud_share` | ge | 0.25 | 0.4777 / 0.4413 / 0.3676 | 0.1101 | 0.1176 | **re-set down** with the area: each cloud is now one card standing alone, so the non-cloud fringe is a bigger share of the green |
| `pose_error_max` | lt | 0.50 | 0 / 0 / 0 | 0 | 0.50 | `GasBankStills.PoseBar` |

Why the ceiling is `lt` 15 and not `le`: the count's own spread across three frames is 4 (11 / 9 / 7), so the
spread check needs a bar of 15 or more, while the goal needs the old build's 15 to fail. `lt` 15 is the only bar
that does both - strictly under 15, clearing this build's max by its full spread.

## The capture recipe

Per run, with this checkout's editor closed and the project free (`python Tools/editor_lock.py status`), two batch
passes into the SAME directory at the SAME wait, so the plate hits the shot's frame:

```bash
# gas pass, then the same line again with TW_GAS_PLATE=1
TW_STILLS_DIR=%TEMP%/tw-gasbank-g2f2 TW_GAS_WAIT_FRAMES=215 \
python Tools/pipeline/run_detached.py start gas-g2f2 --timeout 2400 -- \
  "C:/Program Files/Unity/Hub/Editor/6000.0.50f1/Editor/Unity.exe" -batchmode \
  -projectPath <checkout>/trench-warfare-3d -runTests -testPlatform EditMode \
  -testFilter GasBankStills -testResults <out>/stills-f2.xml -logFile <out>/stills-f2.log
python docs/evidence/ability-fx-captures-r2-2-g2-g2-g2-g1-g1-g2/probe.py --pack %TEMP%/tw-gasbank-g2f2 --into run2
```

Runs 1 and 3 are the same at `TW_GAS_WAIT_FRAMES` 200 and 230. Only one batch Unity instance may run against this
checkout at a time (they all point at the same `-projectPath`): the gas and plate passes, and each run, one after
another, never in parallel.

The `cards/` set is one launch, no wait knob and no plate pass (the test shoots its own plate):

```bash
TW_STILLS_DIR=%TEMP%/tw-gasbank-cards-g2 python Tools/pipeline/run_detached.py start cards-g2 --timeout 2400 -- \
  "C:/Program Files/Unity/Hub/Editor/6000.0.50f1/Editor/Unity.exe" -batchmode \
  -projectPath <checkout>/trench-warfare-3d -runTests -testPlatform EditMode \
  -testFilter GasBankCards -testResults <out>/cards.xml -logFile <out>/cards.log
python docs/evidence/ability-fx-captures-r2-2-g2-g2-g2-g1-g1-g2/probe.py --pack %TEMP%/tw-gasbank-cards-g2 --into cards
```
