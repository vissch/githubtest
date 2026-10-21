# ability-fx-captures-r2-2-g2-g2-g2-g1: a floor under the height, and the puff cut to fit it, 2026-10-22

**`python probe.py` clears, exit 0.** Three fresh captures of this branch's build (`GasPuffStand` 0.32), at three
different frames, every bar outside their spread.

## The owner's complaint

> At the close camera, paint one gas volume like cards-smoke.jpg: a puff with a rim and a height, not the pale
> green stripes on the ground and not the flat puddle in cards-gas.jpg. The height bar is a ceiling of 0.15 and
> the sheet at 0.0116 passes it. Give height a floor the sheet fails, and reshoot three captures. Start from
> `lane/show/ability-fx-captures-r2-2-g2-g2-g2`'s tip. Bar outside the spread of three captures of the current
> build.

He is right about the bar. A one-sided ceiling only refuses a puff that is too TALL: the old flat sheet read
`green_height_frac` 0.0116 and sailed under 0.15, so the ceiling could never catch "no height at all".

## What is here

- `probe.py` — carried from `-g2-g2-g2` unchanged except for the two-sided bar. `BARS` now maps a bar NAME to a
  (measured key, comparison) pair, so one measure can carry two bars: `green_height_frac` (the owner's ceiling
  0.15, `le`) and `green_height_floor` (`ge` on the same `green_height_frac`). The camera maths, the green mask,
  `largest_blob`, the O240 clouds, `--pack`, `--calibrate`, `--selftest` and the spread check are untouched.
- `run1/`, `run2/`, `run3/` — shot in this unit: the four `GasBankStills` captures (`gas-t1`, `gas-t1-plate`,
  `gas-o240`, `gas-o240-plate`) with sidecars and manifests, at `TW_GAS_WAIT_FRAMES` 200 / 215 / 230, so T1 frames
  804 / 819 / 834 and O240 frames 341 / 356 / 371 — three frames, not one. `pose_error_m` 0 everywhere,
  `books_ready` true, `gas_cards` 774 (t1) / 119-131 (o240), `calls_taken` 1 (t1) / 5 (o240).
- `cards/` — carried from `-g2-g2-g2`, so it still shows the OLD gas card, not the puff this build draws. The look
  report that reads it is UNBARRED; part 2's step 8 reshoots it.
- `probe.json` — all three runs' numbers, frames, the look report and `BAR_VALUES`.

## Why height now has two bars

`green_height_frac` is the median column height of the gas, over the frame height at the gas's distance. A
ceiling alone says "not a wall". A floor says "not a puddle". The sheet's own 0.0116 fails the new floor 0.05
outright, which is the whole point of the owner's note:

    python -c "import json;print(json.load(open('docs/evidence/ability-fx-captures-r2-2-g2-g2-g2/probe.json'))['runs']['run1']['green_height_frac'])"
    0.0116

## The look had to move: GasPuffStand 1 -> 0.32

The puff landed on this branch's tip at `GasPuffStand` 1 (a card as tall as it is wide). Shot and measured, that
is not a puff, it is a WALL: three captures read `green_height_frac` 0.514 / 0.532 / 0.515 against the owner's
ceiling of 0.15, with green over 71% of the frame and `green_low_share` 0.07 against his floor of 0.30. Two
owner-decided bars, both broken. An owner-decided bar is a bar, so the look moved, measured, not guessed — each
step is one `GasBankStills` gas pass plus its plate pass:

| GasPuffStand | green_height_frac | green_low_share | green_rise_m | green_area_frac | verdict |
|---|---|---|---|---|---|
| 1.0 | 0.514 | 0.071 | 10.16 | 0.707 | a wall: over the 0.15 ceiling, under the 0.30 low floor |
| 0.2 | 0.0131 | 1.000 | 0.41 | 0.0070 | a puddle again: under 0.02, so the sheet's 0.0116 could not be refused |
| 0.5 | 0.1440 | 0.206 | 4.13 | 0.1812 | 0.144 shaves the ceiling, and low_share breaks the 0.30 floor |
| 0.3 | 0.0238 | 0.713 | 0.79 | 0.0281 | clears, but only 0.004 of room over 0.02: no floor worth setting |
| 0.35 | 0.0824 | 0.354 | 2.47 | 0.0769 | clears; low_share margin only 0.05 |
| **0.32** | **0.0768** | **0.414** | **2.38** | **0.0520** | **kept: room at both ends of every bar** |

0.32 of the card's width puts the median gas about 2.4 m up: a puff that stands over a crouching man and not over
the camera.

## The bars

Every bar is checked twice: all three runs on the pass side, AND the nearest run value clearing the bar by at
least the three runs' own spread (max - min).

| key | cmp | bar | the three runs | spread | margin | why |
|---|---|---|---|---|---|---|
| `green_low_share` | ge | 0.30 | 0.4136 / 0.4242 / 0.4235 | 0.0106 | 0.1136 | owner-decided: the gas is a man's gas |
| `green_height_frac` | le | 0.15 | 0.0768 / 0.0741 / 0.0757 | 0.0027 | 0.0732 | owner-decided ceiling: not a wall |
| `green_height_floor` | ge | 0.05 | 0.0768 / 0.0741 / 0.0757 | 0.0027 | 0.0241 | NEW: not a puddle; over 0.02, so the sheet's 0.0116 fails |
| `green_area_frac` | ge | 0.04 | 0.0520 / 0.0508 / 0.0511 | 0.0012 | 0.0108 | the puff is there to be seen (0.005 before: that bar was a sheet's) |
| `green_rise_m` | ge | 2.0 | 2.380 / 2.304 / 2.353 | 0.0760 | 0.3040 | the height in metres, not only in frame shares (0.30 before) |
| `o240_green_area_frac` | ge | 0.008 | 0.0093 / 0.0093 / 0.0098 | 0.0005 | 0.0013 | carried: the overview's clouded pixels |
| `o240_cloud_count` | ge | 10 | 20 / 15 / 20 | 5 | 5 | carried: clouds, not a wash |
| `o240_cloud_share` | ge | 0.85 | 0.8681 / 0.8696 / 0.8847 | 0.0166 | 0.0181 | carried |
| `pose_error_max` | lt | 0.50 | 0 / 0 / 0 | 0 | 0.50 | `GasBankStills.PoseBar` |

O240 did not move, by design: outside `BankMergeZoom` 120 the bank is still `GasBankSquash`'s squashed blanket
(step 2 of `BankStep`), and only the close camera draws `Book.GasPuff`. Its numbers are the same as `-g2-g2-g2`'s.

## The capture recipe

Per run, with this checkout's editor closed and the project free (`python Tools/editor_lock.py status`), two batch
passes into the SAME directory at the SAME wait, so the plate hits the shot's frame:

```bash
# gas pass, then the same line again with TW_GAS_PLATE=1
TW_STILLS_DIR=%TEMP%/tw-gasbank-f1 TW_GAS_WAIT_FRAMES=200 \
python Tools/pipeline/run_detached.py start gas-f1 --timeout 2400 -- \
  "C:/Program Files/Unity/Hub/Editor/6000.0.50f1/Editor/Unity.exe" -batchmode \
  -projectPath <checkout>/trench-warfare-3d -runTests -testPlatform EditMode \
  -testFilter GasBankStills -testResults <out>/stills-f1.xml -logFile <out>/stills-f1.log
python docs/evidence/ability-fx-captures-r2-2-g2-g2-g2-g1/probe.py --pack %TEMP%/tw-gasbank-f1 --into run1
```

Runs 2 and 3 are the same at `TW_GAS_WAIT_FRAMES` 215 and 230: three waits are three frames, and the probe refuses
two runs that photographed one frame.
