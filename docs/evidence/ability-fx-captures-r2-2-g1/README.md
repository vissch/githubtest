# ability-fx-captures-r2-2-g1 — the chlorine bank at zoom 30 at night: the before takes and the frozen bars

The owner, after r2-2: *"At zoom 30 at night there is no chlorine bank. gas-t1.jpg and gas-t1-plate.jpg are the
same field, and this pass has no evidence folder of its own. Put a bank in the play frame, shot three times."*

This folder is that pass's own evidence. Part 1 (this commit) shoots the CURRENT build three times, measures it,
and **freezes the bars** before anything is touched. Part 2 (this commit) changed the look (the owner's option (b): grow and brighten the bank) and shot three after
takes against those same bars. **All three clear every bar.**

## The before takes

Three runs of `GasBankStills` on commit `f032514063`, nothing in `Assets/**` changed between them, plus one
no-gas plate from a fourth run. The sim centres the gas at (56, 142), the camera sits at zoom 30, pitch 25 deg,
night (`Atmosphere.PinnedClock = 30`), 1280x720.

| take | `bank_area_frac` | `bank_contrast_p50` | `low_share` | `height_frac` | `pose_error_m` |
|---|---|---|---|---|---|
| t1-before-1 | 0.00128 | 50.57 | 0.9874 | 0.0137 | 0.000 |
| t1-before-2 | 0.00128 | 50.57 | 0.9874 | 0.0137 | 0.000 |
| t1-before-3 | 0.00128 | 50.57 | 0.9874 | 0.0137 | 0.000 |
| **spread (max - min)** | **0.00000** | **0.00** | | | |

The bank the eye can see is **0.128% of the play frame** — about 1,180 pixels, a patch some 34 px square in a
1280x720 frame. That is the owner's complaint in one number: there is a bank in the sim (the manifest of each run
says 774 gas cards drawn, `recipes` 1, `books_ready` true, every call taken), it is drawn, and at the play zoom
at night it is invisible. `t1-before-1.jpg` beside this file shows it: a night field, a wrecked tank, a trench
line — no bank.

The other two numbers are already good and must stay good: what little bank there is lies flat on the ground
(`low_share` 0.9874 of its pixels under 1.8 m, `height_frac` 0.0137). Part 2 must make it *bigger and brighter*,
not taller.

### The spread is zero

The harness waits in FRAME COUNTS, pins the clock, kills shake, the ambient bombardment and the scripted peer, so
three runs of the same commit give the same frame to the pixel in every number this probe takes. That was the
expected outcome (it is named as a risk in the plan) and it is the useful one: run-to-run noise is not what hides
the bank.

## The frozen bars

`baseline.json` holds them. They were computed once, here, from the table above, and **part 2 may not recompute
or move them**:

| bar | value | where it comes from |
|---|---|---|
| `bank_area_bar` | **0.03000** | `max(0.03, max_before + 4 * spread)` = `max(0.03, 0.00128 + 0)` — the floor |
| `contrast_bar` | **50.57** | `max(30.0, max_before + 4 * spread)` = `max(30.0, 50.57 + 0)` — the before take itself |
| `low_share` | >= 0.30 | r2's pooling bar, unchanged |
| `height_frac` | <= 0.15 | r2's pooling bar, unchanged |
| `pose_error_m` | < 0.50 | a still from the wrong pose is worthless (r1's verdict) |

Because the spread is zero, both bars fall back to their own definition's other half. `bank_area_bar` is the
0.03 floor, and it is the bar that bites: it asks for **23 times** the area the current build shows.
`contrast_bar` is simply "no dimmer inside the bank than today": at a 40 lift the pixels that do change already
read strongly (median distance 50.6), so the fault is reach, not brightness of the few pixels that make it.

## Why a 3% floor (the footprint working)

A chlorine patch is about 14 m across. At 50 m from the camera with a 25 deg vertical FOV, the frame is
`2 * 50 * tan(12.5 deg)` = 22.2 m tall and `22.2 * 16/9` = 39.5 m wide. A 14 m patch therefore spans 14/39.5 of
the frame's width, and, lying flat and seen at a 25 deg pitch, `14 * sin(25 deg)` = 5.9 m of its 22.2 m height:
about **9.6% of the frame**. So a 3% bar asks that under a third of the bank's own footprint reads as a visible
lift. It is a modest ask, not a generous one.

## The measure

`probe.py` (its docstring is the full description):

- the mask is **plate difference >= 40** per pixel in RGB distance. r2 used 20; a 20/255 lift on a night field is
  a haze the eye does not read as a bank, and that gap is exactly why r2's numbers were green while the owner saw
  nothing. r2's hue test is gone altogether: the bank arrives as a pale blue-cyan haze at night, and r2-2 already
  closed that as the fault.
- only the **single largest coherent patch** of that mask counts (r2's `largest_blob`), so JPEG noise and moving
  infantry are not counted as gas.
- heights are **metres**, rebuilt from each still's sidecar pose (`ray` / `ground_distance` / `height_at`);
  `python probe.py --selftest` checks that maths against a hand calculation and prints `0.34311`.

## The after takes (part 2)

Three runs of `GasBankStills` on the changed build, plus its own no-gas plate, the same harness and the same
frame as the before takes.

| take | `bank_area_frac` | `bank_contrast_p50` | `low_share` | `height_frac` | `pose_error_m` |
|---|---|---|---|---|---|
| t1-after-1 | 0.03252 | 62.18 | 0.4553 | 0.0578 | 0.000 |
| t1-after-2 | 0.03252 | 62.18 | 0.4553 | 0.0578 | 0.000 |
| t1-after-3 | 0.03252 | 62.18 | 0.4553 | 0.0578 | 0.000 |
| **bar** | **>= 0.03000** | **>= 50.57** | >= 0.30 | <= 0.15 | < 0.50 |
| before | 0.00128 | 50.57 | 0.9874 | 0.0137 | 0.000 |

The bank is **3.25% of the play frame against 0.13%**: 25 times the area the eye can see, and over the frozen
0.03 bar. It is still a pool, not a wall: `height_frac` 0.058 against a 0.15 bar, `low_share` 0.46 against 0.30.
`t1-after-1.jpg` beside `t1-before-1.jpg` is the whole answer to the owner's "there is no chlorine bank".

The spread of the after takes is zero as well, for the same reason as the before takes' (the harness is
frame-locked): the three runs give the same frame to the pixel.

- `plate_drift_px` **593 of 921,600** (0.06%), the before plate against the changed build's own plate
  (`t1-plate2.jpg`). A plate pass skips every gas call, so no gas card is drawn and the look change cannot reach
  it; what moved is JPEG and the men walking. A row, not a bar.
- **Draw calls 131, against 133 before** (the T1 sidecar's `draw_calls`). The change costs nothing: 775 gas cards
  against 774, one packed draw.

## The knobs, and what each did

In the order they were moved. Nothing new was drawn and no book was added: every one is a number or a flag in a
row that already existed.

| knob | from | to | what it did |
|---|---|---|---|
| `FlipbookFx` GasBank `Mood` | 0.45 | 0.15 | how much the night mood tints the bank's shade. At 0.45 the chlorine sank into the blue field. |
| `FlipbookFx` GasBank `Lit` | 0.75 | 1.00 | the lit half of the drawing at full strength, so the bank's top face reads green. |
| `FlipbookFx` GasBank `High` | 0.60 | 0.80 | widens the drawing's light band: more of the ink reads as lit, less as shade. |
| `CombatFx.GasBankNight` | 0.74, 0.80, 0.34 | 0.86, 0.92, 0.52 | a brighter, less saturated night green (`ApplyTints` overwrites the sheet's own `Tint`, so this is where the colour lives). |
| `CombatFx.GasBankThin` | 0.60 | 0.85 | a thin cell's card is nearly as opaque as a full one's: the bank's edge reads too. |
| `DrawGasBank` card width | `5 + 2 * Thick` | `6 + 2.5 * Thick` | each card covers more ground, so the cells join into a blanket instead of dotting it. |
| `DrawGasBank` `Refresh(full)` | 14 | 10 | what counts as a thick cell, so more cells draw at the full alpha. |
| `FlipbookFx` GasBank `Ground` | (unset) | **true** | **the one that mattered.** |

The first seven together took the bank from 0.00128 to 0.01088 of the frame: better, and still a third of the
bar. `Ground` took it to 0.03252 on its own.

### Why `Ground` was the one

`Flipbook_URP.shader` softens every card where it meets the scene behind it:

```hlsl
alpha *= lerp(saturate((scene - i.extra.y) / 0.8), 1.0, _Ground) * ...
```

A card is faded out over the last 0.8 m before whatever stands behind it, so a cloud does not cut a hard line
into the ground. A bank that **lies on the ground** is behind nothing: `DrawGasBank` anchors it at
`Ground - 0.35`, its own depth and the ground's depth are the same, and that lerp took almost all of its alpha
away. `_Ground` is the shader's own exemption for exactly this case, and the sheet flag that sets it
(`Sheet.Ground`) was never set on GasBank. `GroundRing` has carried it all along.

So the bank was not dim, and it was not small: it was being erased by a softening meant for clouds in the air.
That is a better answer than any of the brightness knobs, and it is why the look change is cheap.

### What the owner should look at

The bank now reads at zoom 30 at night, and it reads as flat green **patches** lying on the mud rather than as
one rolling cloud: it is the GasBank sheet's own drawing, seen nearly from above at a 25 degree pitch and
squashed to 0.55, with its soft edge no longer eaten by the ground. Softening it again (card alpha 0.55/0.75,
tried and measured) only dims the same shapes - 0.024 of the frame, under the bar, no rounder - so it is the
sheet's drawing that decides the shape, not a number. If the owner wants a rolling cloud instead of patches,
that is a new sheet (tw-vfx-sheets), not a knob, and it should be asked as its own brief.

### The guard that moved

`BurstRecipeTests.TheGasBankLiesOnTheGround_NotOverAMansHead` asked its question of a 7 m card. The widest card
is now `(6 + 2.5) * 1.1` = 9.35 m, so the test asks it of 9.35 m: the gas tops out at **1.71 m**, still under a
1.8 m man's head, and still over the 0.6 m floor that keeps it from being a painted line. The number and its
comment moved with the code, in the same commit.

## Running it

```bash
python docs/evidence/ability-fx-captures-r2-2-g1/probe.py --selftest
python docs/evidence/ability-fx-captures-r2-2-g1/probe.py --baseline <d1> <d2> <d3> --plate <d>   # part 1, done
python docs/evidence/ability-fx-captures-r2-2-g1/probe.py --after <d1> <d2> <d3>                   # part 2
python docs/evidence/ability-fx-captures-r2-2-g1/probe.py --plate-drift <d>                         # the plate row
python docs/evidence/ability-fx-captures-r2-2-g1/probe.py                                          # the verdict
```

A capture dir is made by one batch run of the harness:

```bash
TW_STILLS_DIR=$TEMP/tw-g1-after-1 Unity.exe -batchmode -projectPath <checkout>/trench-warfare-3d \
  -runTests -testPlatform EditMode -testFilter GasBankStills -testResults <xml> -logFile <log>
```

(add `TW_GAS_PLATE=1` for a plate pass; check the log's `GAS pass into` line names the dir you meant, or the
stills land in `%TEMP%/tw-gasbank` and overwrite each other.)

`python docs/evidence/ability-fx-captures-r2-2-g1/probe.py` now exits **0**, and writes `probe.json`. (Part 1
left it red, with "missing stills: t1-after-1..3"; that was correct, there was no after build yet.)

The O240 overview stills the harness also shoots are out of scope here — the owner named zoom 30.
