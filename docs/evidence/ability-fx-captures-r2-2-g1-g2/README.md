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
1057, 1102, and the plate pass photographed the **same ticks and the same frames**, with `gas_cells` 0.

## The before takes (the current build, `be5e6444fc`)

| take | sim tick | `gas_cells` | `bank_area_frac` | `bank_fill` | `bank_contrast_p50` | `low_share` | `bank_height_m` | `stripe_frac` | `pose_error_m` |
|---|---|---|---|---|---|---|---|---|---|
| t1-before-1 | 695 | 599 | 0.13454 | 0.1422 | 56.55 | 0.9891 | 0.000 | 0.00000 | 0.000 |
| t1-before-2 | 725 | 505 | 0.13541 | 0.1432 | 55.91 | 0.9938 | 0.000 | 0.00000 | 0.000 |
| t1-before-3 | 755 | 402 | 0.13038 | 0.1378 | 55.23 | 0.9983 | 0.000 | 0.00000 | 0.000 |
| **spread (max - min)** | | | **0.00503** | **0.0054** | **1.32** | | | | |

**The spread is no longer zero.** Three takes, three ticks, three different gas fields (599, 505 and 402 live
cells), so a bar can sit outside run-to-run noise instead of on top of one frame.

## The frozen bars

`baseline.json` holds them; part 2 may not recompute or move them.

| bar | value | where it comes from |
|---|---|---|
| `bank_area_bar` | **0.15553** | `max(0.03, 0.13541 + 4 * 0.00503)` — four spreads above the best before take |
| `bank_fill_bar` | **0.35000** | `max(0.35, 0.1432 + 4 * 0.0054)` — the 0.35 floor |
| `contrast_bar` | **61.83** | `max(30.0, 56.55 + 4 * 1.32)` — four spreads above the best before take |
| `low_share` | >= 0.85 | the bank lies on the ground inside its own footprint |
| `bank_height_m` | <= 1.00 | a bank, not a wall; one-sided, so depth is not height |
| `pose_error_m` | < 0.50 | a still from the wrong pose is worthless (r1's verdict) |

## The window is too wide — an open question for part 2

The measure is right; **the footprint the plan gave it is not.** The window is the disc of
`R = sqrt(gas_cells * 16 / pi)` round the gas centre, clamped to 7..40 m. `gas_cells` is the **whole field's**
live cell count, and on this build it is 599 cells = 9,584 m², i.e. R = 55 m, clamped to 40 m. A 40 m disc at
zoom 30 is wider than the frame, so:

- `window_px` is **871,680 of 921,600** — the window is 95% of the frame;
- `stripe_frac` is therefore **0.00000**: the stripes the owner complained about are back inside the "bank";
- `bank_area_frac` 0.1345 is the whole mask again, and `bank_area_bar` 0.1555 asks part 2 to make that mask
  *bigger* — which is the g1 mistake repeated.

The footprint has to be sized by the gas **near the camera's focus**, not by every live cell on the map. That is a
change to what the measure counts, so it is written up as an open row in `docs/reference/decisions.md` with the
default (count only live cells within 25 m of the gas centre) rather than decided here.

`low_share` 0.989 and `bank_height_m` 0.000 are honest either way: whatever the window's width, the paint's ground
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
