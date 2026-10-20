# ability-fx-captures-r2-2-g1-g1 — one soft bank, not a zebra: the before takes and the frozen bars

The owner, after g1: *"At zoom 30 at night the play frame is a zebra of flat yellow-green cards, not a chlorine
bank. t1-after-1.jpg and t1-after-2.jpg are the same file, and t1-after-3.jpg is that same frame. The stripes run
from the tank across the trench and into the foreground. t1-plate2.jpg is the field with the paint gone. Draw one
soft bank in one place, a volume a player can point at."*

Part 1 (this commit) answers the second half of that at once — the harness now really shoots three times — and
**freezes the bars** off the current build before anything about the look is touched. The probe is red here
("missing stills: t1-after-1..3"), and that is correct: there is no changed build yet.

## The three takes are now three frames

g1 shot the T1 band **once** and its probe packed that one still three times, so "t1-after-1.jpg and
t1-after-2.jpg are the same file" was literally true, and g1's "spread 0.00000" was not a measurement of noise at
all. `GasBankStills` now shoots the T1 block three times, **9 frames apart** (`TakeGap`), as `gas-t1`,
`gas-t1-b`, `gas-t1-c`, and the plate pass shoots the same three stems with `-plate`, so **every take is
subtracted against its own moment** of the field. One capture dir now holds three takes, which is why `probe.py`
takes one directory per pass and not three.

The block pad went 520 -> 574 frames (2 * 9 waiting, 2 * 12 photographing, 36 spare for a retried shot) so the
gas pass and the plate pass still hit the same frames, and so the same sim ticks.

## The before takes

Three takes from one run of `GasBankStills` on commit `be5e6444fc`, plus their three plates from one plate run.
Same frame as g1: zoom 30, pitch 25 deg, night (`Atmosphere.PinnedClock = 30`), 1280x720.

| take | `bank_area_frac` | `stray_frac` | `fill_frac` | `spread_m` | `rim_px` | `low_share` | `height_frac` | `contrast` | `pose_error_m` |
|---|---|---|---|---|---|---|---|---|---|
| t1-before-1 | 0.01815 | 0.8939 | 0.3024 | 35.63 | 2.00 | 0.8360 | 0.0214 | 60.07 | 0.000 |
| t1-before-2 | 0.01831 | 0.8922 | 0.3056 | 35.58 | 2.00 | 0.8344 | 0.0211 | 60.14 | 0.000 |
| t1-before-3 | 0.03192 | 0.8127 | 0.2973 | 34.92 | 2.00 | 0.4580 | 0.0538 | 62.01 | 0.000 |
| **spread** | **0.01377** | **0.0812** | **0.0083** | **0.71** | **0.00** | | | | |

The spread is no longer zero, so the bars stand on something: the cloud really does boil between the takes
(take 3 is a thicker moment — twice the area, half of it over a man's head).

And the numbers say "zebra" out loud:

- **`stray_frac` 0.81-0.89.** Nine pixels in ten that the eye reads as gas lie **outside** the largest patch.
  That is the owner's "the stripes run from the tank across the trench and into the foreground", in one number.
- **`fill_frac` 0.30.** Even the largest patch fills less than a third of its own bounding box: stripes with
  gaps, not a body.
- **`spread_m` 35 m.** The patch is spread over 35 m of ground. A chlorine patch is 14 m across.
- **`rim_px` 2.00** on every take. The ink goes from full lift to plate in two pixels — a card edge, a step.
  Nothing fades.

## The bars

`baseline.json` holds them. They were computed once, here, and **part 2 may not recompute or move them.**

| bar | value | where it comes from |
|---|---|---|
| `bank_area_frac` | **0.010 .. 0.090** (a band) | absolute, both ends. See "why the area is a band" below |
| `stray_bar` | **<= 0.30** | `min(0.30, 0.8127 - 4 * 0.0812)` = the 0.30 ceiling. Nine pixels in ten that read as gas today lie outside the bank; at most three in ten may after |
| `fill_bar` | **>= 0.45** | `max(0.45, 0.3056 + 4 * 0.0083)` = the 0.45 floor. Under half a solid patch: an ask a soft blob meets and a comb does not |
| `spread_bar_m` | **<= 32.08** | `min(35, 34.92 - 4 * 0.71)` — 14 m of patch plus its drift, and under today's 35 m by four spreads |
| `rim_bar_px` | **>= 3.00** | `max(3.0, 2.00 + 4 * 0.00)` = the 3 px floor. More than a card's step; the softest honest ask |
| `low_share` | >= 0.30 | r2's pooling bar, unchanged |
| `height_frac` | <= 0.15 | r2's pooling bar, unchanged |
| `pose_error_m` | < 0.50 | a still from the wrong pose is worthless (r1's verdict) |

`bank_contrast_p50` is a **row, not a bar**, in this pass. g1 proved the lift (60-62 against a 40 mask); softening
a rim costs brightness at its edge by definition, and the owner asked for a shape, not for more light.

### Why the area is a band, and why the spread rule is not applied to it

Every other bar is a direction: a number that must rise sits four run-to-run spreads above the worst before take,
one that must fall four below the best. `bank_area_frac` is the one number with no direction — too small is
invisible, too big is paint — so it is frozen as the plan's absolute band and the spread rule is deliberately not
applied to it:

- **0.010** is a 96 px square in a 1280x720 frame: about the smallest patch the eye finds at a glance. It is also
  below where the build sits today (0.018), on purpose: this pass may *lose* painted area as the stripes merge,
  and that is a pass, not a failure. The gas that kills is still the sim's field; the drawing is the core of it.
- **0.090** is a bank's own footprint, g1's working: a 14 m patch at 50 m, with a 25 deg vertical FOV, sees a
  frame `2 * 50 * tan(12.5 deg)` = 22.2 m tall and 39.5 m wide, and lying flat under a 25 deg pitch it covers
  `14 / 39.5` of the width by `14 * sin(25 deg) / 22.2` of the height — about **9.6%**. More than that cannot be
  one 14 m patch; it is paint.

Pushed up by four spreads instead, the band would have been **0.087 .. 0.090**: a window 0.003 wide that can only
be hit by painting *more* field. That is the complaint, not the fix.

## The measure

`probe.py` (its docstring is the full description). From g1, unchanged: the plate-difference mask at **40** RGB
distance, `largest_blob`, the camera maths rebuilt from each sidecar (`--selftest` prints `0.34311`). New here:

| number | what it is | what it catches |
|---|---|---|
| `stray_frac` | masked px outside the largest blob / all masked px | paint spread over the field |
| `fill_frac` | blob px / its bounding-box px | a comb of stripes with gaps |
| `spread_m` | the blob's box across the **ground**, in metres, at its own median row | "one place", in metres |
| `rim_px` | 180 rays from the centroid: the last radius at >= 40 lift, the first past it under 10, median gap | a hard card edge |

## Running it

```bash
python docs/evidence/ability-fx-captures-r2-2-g1-g1/probe.py --selftest
python docs/evidence/ability-fx-captures-r2-2-g1-g1/probe.py --baseline <gas dir> --plate <plate dir>  # part 1, done
python docs/evidence/ability-fx-captures-r2-2-g1-g1/probe.py --after <gas dir> --plate <plate dir>     # part 2
python docs/evidence/ability-fx-captures-r2-2-g1-g1/probe.py                                          # the verdict
```

A capture dir is made by one batch run of the harness, from the repo root:

```bash
TW_STILLS_DIR=$TEMP/tw-g1g1-gas Unity.exe -batchmode -projectPath <checkout>/trench-warfare-3d \
  -runTests -testPlatform EditMode -testFilter GasBankStills -testResults <xml> -logFile <log>
```

(add `TW_GAS_PLATE=1` and a different `TW_STILLS_DIR` for the plate pass; check the log's `GAS pass into` line
names the dir you meant, or the stills land in `%TEMP%/tw-gasbank` and overwrite each other.)

The O240 overview stills the harness also shoots are out of scope here — the owner named zoom 30.

---

# Part 2: the change, the six builds it took, and where it stands

**The zebra is gone and the bank is one place.** `t1-after-1..3.jpg` against `t1-before-1..3.jpg`: the paint is one
patch over the trench, 4.3% of the frame, 31 m across, its rim fading over 3 px, instead of 670 columns of flat
lozenges reaching from the tank into the foreground. **Two of the frozen bars are still red** (`fill_frac`,
`stray_frac`), the bars were not moved, and the row is open for the owner in
`docs/reference/decisions.md` ("the GasBank sheet cannot fill it").

## The after takes

| take | `bank_area_frac` | `stray_frac` | `fill_frac` | `spread_m` | `rim_px` | `low_share` | `height_frac` | `contrast` |
|---|---|---|---|---|---|---|---|---|
| t1-after-1 | 0.04288 | 0.3293 | 0.2864 | 31.25 | 3.00 | 0.2850 | 0.1126 | — |
| t1-after-2 | 0.04280 | 0.3170 | 0.2841 | 31.30 | 3.00 | 0.3023 | 0.1028 | — |
| t1-after-3 | 0.04337 | 0.3102 | 0.2853 | 31.44 | 3.00 | 0.2804 | 0.1119 | — |
| bar | 0.010 .. 0.090 ✓ | <= 0.30 ✗ | >= 0.45 ✗ | <= 32.08 ✓ | >= 3.00 ✓ | >= 0.30 ~ | <= 0.15 ✓ | row |

Against the before takes: area 0.018 -> 0.043 (inside the band at both ends), stray 0.89 -> 0.32, spread 35.6 ->
31.3, rim 2.0 -> 3.0, fill 0.30 -> 0.285. The three takes are 9 frames apart and agree to 0.0006 in area, so the
numbers are the build's, not a moment's.

## What each knob did (nine measured builds, three takes each; every cell is that build's median take)

| build | change | area | stray | fill | spread m | rim px |
|---|---|---|---|---|---|---|
| before | — | 0.018 | 0.89 | 0.30 | 35.6 | 2.0 |
| 1 | relative floor 0.45 of peak, alpha 0.10-0.75 smoothstep, roll, anchor +0.1, sheet `Ground` 0.55 | 0.0018 | 0.92 | 0.51 | 3.3 | 3.0 |
| 2 | + thickness read against the field's own peak | 0.0015 | 0.92 | 0.46 | 3.0 | 3.0 |
| 3 | floor 0.15 of peak, thickness from the floor up, alpha 0.20-0.75 | 0.0015 | 0.92 | 0.45 | 3.1 | 3.0 |
| 4 | + **one place**: only the largest connected run of standing cells, alpha 0.35-1.0 | 0.0021 | 0.90 | 0.59 | 3.2 | 3.0 |
| 5 | cards 13-18.5 m wide (were 6-8.5), squash 0.25 | 0.0047 | 0.82 | 0.58 | 7.4 | 3.0 |
| 6 | cards 22-31 m, squash 0.15, alpha 0.50-1.0 | 0.0227 | 0.50 | 0.23 | 25.9 | 3.0 |
| 7 | + **3 puffs a cell**, layered and offset | 0.066 | 0.22 | 0.31 | 36.8 | 3.0 |
| 8 | 4 puffs, cards 17-24 m, squash 0.18 | 0.060 | 0.23 | 0.30 | 33.5 | 3.0 |
| **shipped** | **3 puffs at half-cell offsets, cards 15-21 m, squash 0.20** | **0.043** | **0.32** | **0.285** | **31.3** | **3.0** |
| (tried) | the same with the floor at 0.35 of peak | 0.015 | 0.54 | 0.386 | 17.4 | 2.0 |

Read it as two walls. Going **narrow** (a high floor, few cards) the patch is small, round and separate: `fill`
climbs to 0.386 but the paint breaks into pieces and `stray` doubles. Going **wide** (layered puffs) the paint is
one body and `stray` falls to 0.21, but the body is a ragged diagonal lens in an axis-aligned box, so `fill` stalls
near 0.30. No setting of floor, alpha, width, squash or layer count put both over their bars at once.

**Why:** the `GasBank` sheet draws one small eroded puff in the middle of its card. A bank can only be made of many
such puffs, and many puffs leave holes. Filling them needs a sheet whose frame *is* a rolling body — a new sheet
(tw-vfx-sheets), which g1 already told the owner is not a knob. That is the open row.

## The knobs that shipped

| knob | before | after | why |
|---|---|---|---|
| `GasBankPeakFrac` | — (an absolute 0.8 floor) | 0.15 of the field's own peak, never under 0.8 | the core is the bank; the 4%/tick diffusion tail is not. The absolute floor stays as the lower bound, so what kills is always at least outlined |
| one place | — | only the largest connected run of standing cells | a player can point at a bank; he cannot point at a field's whole spread |
| thickness | `c / 10` (a `full` the field never reaches: every card read as thin) | `(c - floor) / (peak - floor)` | the drawn cells use the whole alpha range |
| `GasBankThin`/`Full` | 0.85 / 1.00, a lerp | 0.50 / 1.00, a `Mathf.SmoothStep` | the rim fades instead of ending on a card edge (`rim_px` 2 -> 3) |
| roll | 0 | per card, over the full turn, from its hash | the repeated lozenge stops lining up into stripes |
| card width | 6-8.5 m | 15-21 m | puffs on a 4 m grid must overlap to be a body |
| `GasBankSquash` | 0.55 | 0.20 | wider cards, same 1.54 m of gas: still under a man's head (`BurstRecipeTests`) |
| `GasBankPuffs` | 1 a cell | 3 a cell, offset half a cell, each its own roll and frame | the sheet's puff is smaller than its card |
| anchor | `Ground - 0.35` | `Ground + 0.1` | the ground soften needs the card's foot in frame |
| `Sheet.Ground` | `bool`; GasBank 1 (no soften) | `float`; GasBank 0.55, GroundRing 1 | the bank meets the mud in a fade, without the erasure that made it invisible before g1 |

## Cost

The gas book is still **one packed draw** (`books.DrawPacked`), before and after, so the frame's draw-call count is
unchanged. What moved is the card count inside it: the whole field's standing cells before, now the core's cells
(38 at the captured tick) times 3 puffs — about 114 cards, far under `FlipbookFx.MaxCards` (1536). The one-place
pass walks the field once a sim tick with two scratch arrays grown once and reused: no allocation a frame.

## Guards

`BurstRecipeTests.TheGasBankLiesOnTheGround_NotOverAMansHead` carries the new widest card (23.10 m at squash 0.20 =
1.54 m of gas). `TheGasBankFadesAtItsRim_NotACardEdge` is new: it holds the alpha's two ends, that it is a
smoothstep and not a line, and that the relative floor never goes under the absolute one.
