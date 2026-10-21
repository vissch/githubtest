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

# Part 2: the look against the frozen bars

Five of the six bars are cleared with room. `bank_height_m` is not, and the reason is in the numbers below: it is
not the bank standing up, it is the far field's gas sitting in the top of the window box. That needs the owner.

## The sheet, frame by frame (step 1)

`GasBank.png` is 8 x 4 cells of 256 px, 30 frames. Per frame, over the whole cell and over its bottom third
(the drawing's own band), measured read-only off the PNG:

| frame | mean alpha | share >= 0.45 | share >= 0.10 | bottom third: mean | bottom third: >= 0.45 |
|---|---|---|---|---|---|
| 0 | 0.061 | 0.057 | 0.085 | 0.182 | 0.169 |
| 5 | 0.094 | 0.088 | 0.125 | 0.280 | 0.262 |
| 10 | 0.131 | 0.126 | 0.157 | 0.387 | 0.374 |
| 15 | 0.157 | 0.154 | 0.180 | 0.468 | 0.457 |
| 17 | 0.167 | 0.163 | 0.191 | 0.497 | 0.486 |
| 20 | 0.175 | 0.168 | 0.207 | 0.520 | 0.500 |
| 22 | 0.179 | 0.171 | 0.215 | 0.534 | 0.510 |
| 24 | 0.183 | 0.173 | 0.223 | 0.544 | 0.516 |
| 26 | 0.181 | 0.169 | 0.231 | 0.539 | 0.502 |
| 28 | 0.180 | 0.165 | 0.238 | 0.536 | 0.491 |
| 29 | 0.178 | 0.163 | 0.240 | 0.530 | 0.484 |

Mean alpha peaks at frame 24 and then flattens, but the **ink's coverage** — the share of the cell at or above
alpha 0.10, which is what fills a footprint — climbs the whole way, 0.191 at frame 17 to 0.240 at frame 29. So
`GasBankFirst/Last` moved from 17..28 to **22..29**: the window sits at the dense end of the book instead of
across the growing puff. `BankFrame` sweeps a sine between the two, so what matters is the middle, 25.5 instead
of 22.5.

## The two ceilings that decided the approach

- **The disc ceiling.** The window is 1104 x 522 = 576,288 px and does not move (`gas_cells_near` is 124 in all
  three takes: the 25 m disc is saturated). Of that box, 405,916 px (**0.7044**) lie inside the footprint disc. So
  fill 0.70 is the ceiling and the 0.35 bar wants half the disc painted; 0.196 of it was painted before.
- **The brightness ceiling.** Scaling every pixel's lift over the plate by k and re-counting at `DIFF_BAR` 40, on
  the before takes: k=1.3 gives fill 0.212, k=2.0 gives 0.251, k=3.0 gives 0.274. The sheet's ink covers only
  ~27% of a card at any opacity, so **brightness alone cannot clear the fill bar**. Coverage had to roughly
  double, and overlap was the lever.

## What changed (knob by knob)

| knob / line | was | is | why |
|---|---|---|---|
| `CombatFx.GasBankFirst/Last` | 17 / 28 | **22 / 29** | the sheet's densest run, measured above |
| `DrawGasBank` card width | `(6 + 2.5 * Thick) * (0.9 + 0.2 * h2)` | **`(7.8 + 0.7 * Thick) * (0.9 + 0.2 * h2)`** | the same 9.35 m widest card (so the gas still tops out at 1.71 m and `BurstRecipeTests` is untouched), mean card ~7.5 m -> ~9.1 m, per-card ground footprint up ~1.45x |
| `DrawGasBank` cards per cell | 1 | **2**, offset up to +-2 m in x and z from spare hash bits, boiling 2.1 rad out of phase, mirror flipped, guarded against `MaxCards` | the only lever that lifts coverage past the ink's 27% |
| second card's anchor | - | `Ground - 0.95` (the first card keeps the owner's `Ground - 0.35`) | an attempt to buy height margin back; it did not work, see below |
| `CombatFx.GasBankThin` | 0.85 | **1.00** | round 1 cleared every bar but contrast (60.17 against 64.59) |
| `CombatFx.GasBankNight` | 0.86, 0.92, 0.52 | **0.95, 1.00, 0.60** | the same |

Frozen and untouched: `GasBankSquash` 0.55, the first card's -0.35 anchor, `GasBankStandFloor` 0.8 (the owner's
row of 2026-10-20).

## The after takes

Round 1 (coverage only: frames, width floor, second card):

| take | `bank_area_frac` | `bank_fill` | `bank_contrast_p50` | `low_share` | `bank_height_m` | `gas_cards` | `draw_calls` |
|---|---|---|---|---|---|---|---|
| t1-after-1 | 0.25014 | 0.4000 | 60.17 | 0.9298 | 0.947 | 1194 | 126 |
| t1-after-2 | 0.24884 | 0.3979 | 59.28 | 0.9307 | 0.927 | 1000 | 121 |
| t1-after-3 | 0.23957 | 0.3831 | 58.29 | 0.9332 | 0.757 | 798 | 122 |

Round 2 (the above plus `GasBankThin` 1.0 and the brighter night green) — **the committed build**:

| take | sim tick | `bank_area_frac` | `bank_fill` | `bank_contrast_p50` | `low_share` | `bank_height_m` | `stripe_frac` | `pose_error_m` | `gas_cards` | `draw_calls` |
|---|---|---|---|---|---|---|---|---|---|---|
| t1-after-1 | 695 | 0.22993 | 0.3677 | 70.72 | 0.9225 | **1.368** | 0.07890 | 0.000 | 1194 | 127 |
| t1-after-2 | 725 | 0.23028 | 0.3683 | 69.49 | 0.9245 | **1.312** | 0.07517 | 0.000 | 1000 | 122 |
| t1-after-3 | 755 | 0.22528 | 0.3603 | 67.75 | 0.9283 | **1.119** | 0.07161 | 0.000 | 798 | 122 |
| bar | | >= 0.12063 | >= 0.35 | >= 64.59 | >= 0.85 | <= 1.00 | row | < 0.50 | | <= 133 |

Same three sim ticks as the before takes (695, 725, 755) at the same three frames (1012, 1057, 1102), each against
its own plate, so the plates were not reshot. Draw calls **121-127 against the 133 of g1** — two cards a cell cost
nothing, because it is still one `DrawPacked`. `t1-after-2`'s sidecar carries the "men barely separate from the
ground: unreadable" warning (the plates carry it too; it is a row, not a bar).

Bar against spread: the before spread was area 0.00327, fill 0.0052, contrast 1.61. Area is **36 spreads** above
its bar, fill 2 above, contrast 2 above. The look is outside run-to-run noise on the three bars it clears.

## Why `bank_height_m` is the one bar left, and why it is a question and not a knob

`bank_height_m` is the p95 of each paint pixel's lowest possible gas, and `mins` is exactly 0 for every pixel
whose ground crossing falls inside or in front of the footprint. Measured on the committed takes:

| take | paint px | share with height 0 | share over 1.0 m | p92 | p93 | p95 |
|---|---|---|---|---|---|---|
| t1-after-1 | 211,907 | 0.9225 | 0.0574 | 0.000 | 0.328 | 1.367 |
| t1-after-2 | 212,227 | 0.9246 | 0.0563 | 0.000 | 0.252 | 1.312 |
| t1-after-3 | 207,621 | 0.9283 | 0.0525 | 0.000 | 0.069 | 1.119 |

There is a cliff, not a slope: 92% of the paint is flat on the ground, p93 is a third of a metre, and then the top
5% jumps past a metre. The bar is a p95, so it is decided entirely by whether the share of paint above the
footprint's far rim is under or over 5%. It is 5.2% to 5.7%.

That paint is **not the bank standing up**. The window is the bounding **box** of the footprint disc, so its upper
corners lie above the disc's far rim; the gas beyond 25 m — the same field-wide bank the `stripe_frac` row names —
is drawn there, and every pixel of it is scored as "at least 1.1 m high". Growing the bank grew it too: the before
takes had so little paint anywhere that the share was under 5% and `bank_height_m` read 0.000.

Two things follow:
- Sinking the second band 0.6 m **did not help** (height went 0.947 -> 1.368 with the brightening on top of it):
  the high paint is beyond the footprint, so moving the near cards down cannot reach it.
- Every knob that clears contrast also widens that far-field paint. Measured on round 1's captures by scaling the
  lift by k: contrast 64.59 wants k >= 1.15, and k = 1.08 already puts height at 1.04. **Contrast and height pull
  against each other through the same paint**, so there is no setting of these knobs that clears both.

The bars are frozen and were not moved. What to do about it is the owner's, and it is the open row written into
`docs/reference/decisions.md`.
