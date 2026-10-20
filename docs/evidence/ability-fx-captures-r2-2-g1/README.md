# ability-fx-captures-r2-2-g1 — the chlorine bank at zoom 30 at night: the before takes and the frozen bars

The owner, after r2-2: *"At zoom 30 at night there is no chlorine bank. gas-t1.jpg and gas-t1-plate.jpg are the
same field, and this pass has no evidence folder of its own. Put a bank in the play frame, shot three times."*

This folder is that pass's own evidence. Part 1 (this commit) shoots the CURRENT build three times, measures it,
and **freezes the bars** before anything is touched. Part 2 changes the look (the owner's option (b): grow and
brighten the bank) and shoots three after takes against those same bars.

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

## Running it

```bash
python docs/evidence/ability-fx-captures-r2-2-g1/probe.py --selftest
python docs/evidence/ability-fx-captures-r2-2-g1/probe.py --baseline <d1> <d2> <d3> --plate <d>   # part 1, done
python docs/evidence/ability-fx-captures-r2-2-g1/probe.py --after <d1> <d2> <d3>                   # part 2
python docs/evidence/ability-fx-captures-r2-2-g1/probe.py                                          # the verdict
```

A capture dir is made by one batch run of the harness:

```bash
TW_STILLS_DIR=$TEMP/tw-g1-after-1 Unity.exe -batchmode -projectPath <checkout>/trench-warfare-3d \
  -runTests -testPlatform EditMode -testFilter GasBankStills -testResults <xml> -logFile <log>
```

(add `TW_GAS_PLATE=1` for a plate pass; check the log's `GAS pass into` line names the dir you meant, or the
stills land in `%TEMP%/tw-gasbank` and overwrite each other.)

**Part 1 leaves the plain `probe.py` red**, with "missing stills: t1-after-1, t1-after-2, t1-after-3". That is
correct: there is no after build yet.

The O240 overview stills the harness also shoots are out of scope here — the owner named zoom 30.
