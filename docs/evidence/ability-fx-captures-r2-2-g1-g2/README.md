# ability-fx-captures-r2-2-g1-g2 — the chlorine bank at zoom 30 at night: a measure the owner's words can hold

The owner, on g1's green numbers:

> *"The probe calls 29975 pixels one bank. That is 3.25% of the frame and it clears a bar of 0.030 by 0.00252,
> while the stripes cover the field. The three after takes are one frame, so the spread is zero. contrast_bar
> 50.57 is the before take's own contrast. low_share fell from 0.9874 to 0.4553 and the mask rose from 0.39 m to
> 1.798 m. Count the paint a player sees, keep it on the ground, shoot three captures that can differ, and put the
> bar outside that spread."*

Part 1 (this commit) rebuilds the measure and the harness, shoots the **current build** (g1's tip, `be5e6444fc`)
three times, and freezes the bars. Part 2 changes the look against them.

## What part 1 fixed

| the owner's word | what it was | what it is now |
|---|---|---|
| "29975 pixels one bank" | `largest_blob` bridged the mask through one-pixel gaps and chained the field-wide gas stripes into one 690-column "blob" | `largest_blob` is **deleted**. Paint is the mask inside the bank's own **footprint window** only |
| "the stripes cover the field" | counted as bank | the `stripe_frac` **row**: mask pixels outside the window, named, never a bar |
| "the three after takes are one frame" | three stills of one frame, spread 0 | the harness shoots the T1 band at **three frames 45 apart** (30 sim ticks), `gas-t1`, `gas-t1-b`, `gas-t1-c`, at offsets fixed from the block's first frame, so the plate pass still hits the same frames |
| "a bar on top of a zero spread" | `contrast_bar` was the before take's own contrast | `--baseline` **exits 1** when the area or the contrast spread is zero |
| "the mask rose from 0.39 m to 1.798 m" | per-column `max(ys)`, so a deeper flat bank read as a taller one | `bank_height_m` is the p95 of each paint pixel's **lowest possible** gas, judged one-sidedly against the footprint's far rim. Depth can no longer read as height |
| "each take against one plate" | one plate for all six takes | three plates, `t1-plate-1..3`, each take against **its own** plate |

The two capture runs prove the frame lock: the gas pass photographed sim ticks **695, 725, 755** at frames 1012,
1057, 1102, and the plate pass photographed the **same ticks and the same frames**, with `gas_cells` 0. Both runs
were reshot on 2026-10-21 from `6da3ac9872` after the footprint was narrowed, and hit the same three ticks again.

| the owner's word | what it was | what it is now |
|---|---|---|
| "while the stripes cover the field" (the second half: the window itself) | the footprint was sized from `gas_cells`, the whole MAP's live cells (599), so R came out at 55 m, clamped to 40, and the window was 95% of the frame with `stripe_frac` 0 | the footprint is sized from `gas_cells_near`, the live cells within **25 m** of the gas centre (124 of them), written by the harness; `stripe_frac` is **0.028**, so the stripes are outside the bank again |

## The before takes (the current build, `6da3ac9872`, reshot 2026-10-21)

| take | sim tick | `gas_cells` | `gas_cells_near` | `bank_area_frac` | `bank_fill` | `bank_contrast_p50` | `low_share` | `bank_height_m` | `stripe_frac` | `pose_error_m` |
|---|---|---|---|---|---|---|---|---|---|---|
| t1-before-1 | 695 | 599 | 124 | 0.10617 | 0.1698 | 58.15 | 0.9505 | 0.000 | 0.02837 | 0.000 |
| t1-before-2 | 725 | 505 | 124 | 0.10755 | 0.1720 | 57.28 | 0.9535 | 0.000 | 0.02785 | 0.000 |
| t1-before-3 | 755 | 402 | 124 | 0.10428 | 0.1668 | 56.54 | 0.9549 | 0.000 | 0.02608 | 0.000 |
| **spread (max - min)** | | | | **0.00327** | **0.0052** | **1.61** | | | | |

**The spread is no longer zero.** Three takes, three ticks, three different gas fields (599, 505 and 402 live
cells on the map), so a bar can sit outside run-to-run noise instead of on top of one frame. The near count is 124
in all three: the 25 m disc round the focus is saturated, which is why the window is the same size in each take and
only the paint inside it moves.

## The frozen bars

`baseline.json` holds them; part 2 may not recompute or move them.

| bar | value | where it comes from |
|---|---|---|
| `bank_area_bar` | **0.12063** | `max(0.03, 0.10755 + 4 * 0.00327)` — four spreads above the best before take |
| `bank_fill_bar` | **0.35000** | `max(0.35, 0.1720 + 4 * 0.0052)` — the 0.35 floor |
| `contrast_bar` | **64.59** | `max(30.0, 58.15 + 4 * 1.61)` — four spreads above the best before take |
| `low_share` | >= 0.85 | the bank lies on the ground inside its own footprint |
| `bank_height_m` | <= 1.00 | a bank, not a wall; one-sided, so depth is not height |
| `pose_error_m` | < 0.50 | a still from the wrong pose is worthless (r1's verdict) |

## The window: closed, with the default taken

The open row of 2026-10-20 asked how wide the bank's footprint is. Option **(a)** is built — the default it named:
the harness counts the live cells within `NearRadiusM` **25 m** of the gas centre (`gas_cells_near`) and the probe
sizes the disc from that, falling back to the whole field's `gas_cells` only when the grid's stride cannot be read
(then the count is `-1`). `CombatFx.GasBankStandFloor` holds the 0.8 floor in one place, so the window can never
count cells the bank does not stand.

What it changed, measured on the same build:

| | whole-field footprint (part 1) | near footprint, 25 m (now) |
|---|---|---|
| R | 40 m (clamped from 55) | 25.1 m |
| `window_px` | 871,680 of 921,600 = **0.946** | 576,288 = **0.625** |
| `stripe_frac` | 0.00000 — the stripes were counted as bank | **0.028** — named, outside the bank |
| `bank_area_frac` | 0.1345 (the whole mask) | 0.1062 (paint in the footprint) |

`--baseline` now **refuses** (exit 1) a before take whose `window_px` is over `WINDOW_MAX_FRAC` 0.70 of the frame,
or whose `stripe_frac` is 0: either means the window is the frame again. The bound sits between the two measured
numbers above. (The plan's 0.60 came from scaling the 40 m window's *area* by `(25/40)^2` = 0.37; the window is the
bounding **box** of a disc seen at a 25 degree pitch, and a box does not shrink that way, so the bound was
re-derived from the two measurements instead of the footprint being changed to fit it.)

`low_share` 0.95 and `bank_height_m` 0.000 are honest either way: whatever the window's width, the paint's ground
crossings fall inside the footprint and nothing is forced up off the ground.

## Running it

```bash
python docs/evidence/ability-fx-captures-r2-2-g1-g2/probe.py --selftest
python docs/evidence/ability-fx-captures-r2-2-g1-g2/probe.py --baseline <gas dir> <gas dir> <gas dir> --plate <plate dir>
python docs/evidence/ability-fx-captures-r2-2-g1-g2/probe.py --after <gas dir> <gas dir> <gas dir>
python docs/evidence/ability-fx-captures-r2-2-g1-g2/probe.py          # the verdict
```

One gas run now makes all three takes, so the same dir is passed three times. A capture dir is one batch run:

```bash
TW_STILLS_DIR='C:\...\tw-g2-before' Unity.exe -batchmode \
  -projectPath <checkout>/trench-warfare-3d -runTests -testPlatform EditMode \
  -testFilter GasBankStills -testResults <xml> -logFile <log>
```

(add `TW_GAS_PLATE=1` for the plate pass; the stills must land in the dir you meant, or they go to
`%TEMP%/tw-gasbank` and overwrite each other.)

`--selftest` checks the camera maths against a hand calculation worked in the docstring: the centre pixel hits the
ground at 10.000 m, one degree up stands 0.34311 m high, and `to_ndc` sends that point back to ndc_y 0.0788.

The O240 overview still the harness also shoots is out of scope — the owner named zoom 30.
