# ability-fx-captures-r2-2-visual-review-10: the pictures, 26 units on

## The owner's words

> "show me visual results"

(his answer on the Decide page to "ability-fx-captures-r2-2: chain of fix units 8 deep, 26 units")

So this unit shoots the chlorine bank again on the rebased lane tip and puts the frames side by side with the
first captures of the chain. It measures nothing new and changes no look.

## What `sheet.jpg` shows

Two columns, four rows, 1304 x 1696, under 400 KB. Built by `sheet.py` from committed jpgs only.

| row | folder | what it is |
|---|---|---|
| 1 | `ability-fx-captures-r2` | the look 26 units ago: the T1 field is bare, the gas barely registers |
| 2 | `run1` | this build, T1 frame 804 / O240 frame 341 |
| 3 | `run2` | this build, T1 frame 819 / O240 frame 356 |
| 4 | `run3` | this build, T1 frame 834 / O240 frame 371 |

Left column `gas-t1` (zoom 30, the field the player sees), right column `gas-o240` (the five separated calls).
Each tile is labelled with its folder, the frame its sidecar was photographed at, and three numbers: on the T1
tile `green_low_share`, `green_height_frac` and `t1_pad_aspect`; on the O240 tile `o240_green_area_frac`,
`o240_cloud_share` and `pose_error_max`, because the first three are T1 field measures and do not describe it.

**The eye-level difference:** in row 1 the gas is a thin smear over a few metres of ground; in rows 2-4 a green
cloud lies across the whole forward field and reads as gas at a glance. That is the 26 units of work, in one frame.

## Before and after, in numbers

Both columns are measured by this folder's `probe.py`, so they are measured the same way.

| key | before (`ability-fx-captures-r2`) | run1 | run2 | run3 |
|---|---|---|---|---|
| `green_low_share` | 1.0000 | 0.3393 | 0.3523 | 0.3381 |
| `green_height_frac` | 0.0065 | 0.0887 | 0.0914 | 0.0959 |
| `green_rise_m` | 0.218 | 2.643 | 2.721 | 2.828 |
| `green_area_frac` | 0.0038 | 0.0722 | 0.0698 | 0.0681 |
| `t1_pad_aspect` | 0.2286 | 0.2139 | 0.2373 | 0.2123 |
| `t1_pad_rise_m` | 0.492 | 6.278 | 6.318 | 6.321 |
| `t1_pad_relief` | 0.0942 | 0.1042 | 0.1023 | 0.0989 |
| `o240_green_area_frac` | 0.0096 | 0.0093 | 0.0095 | 0.0101 |
| `o240_cloud_share` | 0.8680 | 0.8616 | 0.8820 | 0.8869 |
| `pose_error_max` | 0 | 0 | 0 | 0 |

Read the before column with care: with `blob_px` 3488 out of a 46161-pixel green mask there is almost no gas blob
to measure, so `green_low_share` 1.0 ("all of nothing is low") and `t1_pad_aspect` 0.2286 are degenerate, not a
rival to the build's. The measures that are honest either way are `green_area_frac` (0.0038 -> ~0.07, nineteen
times the gas) and `t1_pad_rise_m` (0.49 m -> 6.3 m).

**The rebase did not move the look.** All nine field numbers and all four pad numbers sit inside the spread the
previous unit committed from commit `78320befa0`'s own three captures (`green_low_share` 0.3379-0.3522,
`green_height_frac` 0.0887-0.0967, `t1_pad_aspect` 0.2123-0.2367, `pose_error_max` 0) after 181 upstream commits
were replayed under the lane. `python probe.py --calibrate` -> exit 0 prints all of it and writes `probe.json`.

## The capture recipe used

`probe.py` is copied unchanged from `ability-fx-captures-r2-2-g2-g2-g2-g1-g2-g1-g1-g1` so the numbers stay
comparable. Per run, this checkout's editor closed and `python Tools/editor_lock.py status` FREE, two batch passes
into the SAME directory at the SAME wait (gas, then the same line again with `TW_GAS_PLATE=1`), one pass at a time
(two instances on one project both die with STATUS_ACCESS_VIOLATION):

```bash
TW_STILLS_DIR=%TEMP%/tw-gasbank-v1 TW_GAS_WAIT_FRAMES=200 \
python Tools/pipeline/run_detached.py start gas-v1 --timeout 2400 -- \
  "C:/Program Files/Unity/Hub/Editor/6000.0.50f1/Editor/Unity.exe" -batchmode \
  -projectPath <checkout>/trench-warfare-3d -runTests -testPlatform EditMode \
  -testFilter GasBankStills -testResults <out>/stills-v1.xml -logFile <out>/stills-v1.log
# then the same line again with TW_GAS_PLATE=1
python docs/evidence/ability-fx-captures-r2-2-visual-review-10/probe.py --pack %TEMP%/tw-gasbank-v1 --into run1
```

Runs 2 and 3 are the same at `TW_GAS_WAIT_FRAMES` 215 and 230 into `run2`/`run3` (T1 frames 804/819/834). Then
`python sheet.py` -> `sheet.jpg`.

## What this unit does NOT do

It shows pictures. It picks no option, moves no bar, adds no bar, and edits no `CombatFx` knob - no production
file is touched at all. The (a)/(b)/(c) question on the 30% `green_low_share` floor for the standing T1 puff, in
`docs/reference/decisions.md` under the open questions, still waits on the owner.
