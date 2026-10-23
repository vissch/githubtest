# ability-fx-captures-r2-2-g2-g2-g2-g2-g2-g1-g1: the close gas, narrower and rimmed, 2026-10-23

**`python probe.py` clears, exit 0.** `run1`/`run2`/`run3` — each a gas pass, a no-gas plate pass and its own
`GasBankCards` pass, at `TW_GAS_WAIT_FRAMES` 200/215/230 — are three fresh captures of THIS tree.

**Read the last section before you trust the green.** The bars hold, but the battlefield still does not yet show
the owner the cloud he asked for.

## The owner's words

> At zoom 30 gas-t1 is a carpet of flat green ovals. cards-gas is a speck and two beams, and cards-smoke is the
> only standing cloud. Draw the close gas as that cloud, with a rim, on the battlefield still. Start from
> `lane/show/ability-fx-captures-r2-2-g2-g2-g2-g2-g2-g1`'s tip. Bar outside the spread of three captures of the
> current build.

## What changed

Part 1 of the unit (commit `8b857e8e1e`):

- `CombatFx.PackGasCell` sizes the close (step 1) card from its HEIGHT — as tall as `GasHeadRoom` 1.7 m allows
  (0.75 to 1 of the 2.356 m clamp) and `GasCloudAspect` times that wide — instead of from its width, which gave
  the 6.3-8.4 m by 2.36 m band the owner called a flat oval;
- `GasBankCards.Drawer.Which_Books[0]` is `Book.GasCloud`, the book the field actually draws close up, so the
  isolation still photographs the same card as the battlefield still (it was still on `Book.GasBank`);
- the sheet row and the still rig follow, guarded by
  `BurstRecipeTests.TheGasBankReadsAsAStandingCloud_NotAFlatSheet` and `FlipbookOrdinalTests.EveryRow_HasItsBook`.

This leg (part 2) added one tuning round on top, from the first three captures of part 1's tree:

- `GasCloudAspect` **1.35 -> 1.6**. At 1.35 the bank went see-through — `green_area_frac` fell to 0.0044-0.0049
  against the carpet build's 0.0226-0.0276, the risk the plan named. At 1.6 it reads 0.0116-0.0118.
- the rim is on **five cells in eight** (it was three) at **0.55** of the body's alpha (it was 0.35), to try to
  make "with a rim" measurable in the median green column. It did not work: see below. Card count 1239-1259 of
  `FlipbookFx.MaxCards` 1536.

23 of 23 tests in `BurstRecipeTests|FlipbookOrdinalTests` pass on this tree (batch `-runTests`).

## What is here

- `probe.py` — `-g1`'s probe whole (camera maths, `greenness`, `components`, `largest_blob`, `pack`, the frame
  guard, `measure_cards`, every O240 key, `--selftest`, `--calibrate`, `--pack`), plus two new T1 keys,
  `green_blob_share` and `green_rim_lift`, and `--runs <dir>` so `--calibrate` can read another unit's captures.
- `run1/`, `run2/`, `run3/` — the captures. `probe.json` — their numbers, frames and `BAR_VALUES`.

### The two new keys

- **`green_blob_share`** (`le`): the largest green component over all green pixels at T1. A carpet whose green
  hangs together in one sheet scores high; separate standing clouds with ground between them score low.
- **`green_rim_lift`** (`ge`): per green column of at least 8 pixels, the median luma of its top 30% over the
  median of the rest; the key is the median over columns. "With a rim", as a number.

The carpet build's own three captures, read by this probe
(`python probe.py --calibrate --runs ../ability-fx-captures-r2-2-g2-g2-g2-g2-g2-g1`):

| key | the carpet build | this build | separates? |
|---|---|---|---|
| `green_blob_share` | 0.1531, 0.1530, 0.1877 | 0.0565, 0.0562, 0.0571 | **yes** — the carpet fails the 0.065 bar by 3x |
| `green_rim_lift` | 0.9902, 0.9919, 0.9933 | 1.0005, 0.9839, 0.9845 | **no** — the carpet clears the 0.96 bar |

## The bars

All three runs on the pass side, AND the nearest value clearing the bar by at least the three runs' spread.

| key | cmp | bar | the three runs | spread | margin | the carpet build | why |
|---|---|---|---|---|---|---|---|
| `green_low_share` | `ge` | 0.42 | 0.6499, 0.5478, 0.5392 | 0.1107 | 0.1192 | 0.7393-0.8840 | the head rule, weaker again: a taller cloud puts more over `HEAD_M` 1.8 m |
| `green_rise_m` | `ge` | 0.50 | 0.899, 1.250, 1.289 | 0.3900 | 0.3990 | 0.575-0.735 | the median column's p45 height; does not fail the carpet on its own |
| `green_area_frac` | `ge` | 0.0113 | 0.0118, 0.0116, 0.0116 | 0.0002 | 0.0003 | 0.0226-0.0276 | a floor against the narrow card going see-through, not a look measure |
| `green_knee_share` | `le` | 0.29 | 0.2358, 0.1947, 0.1901 | 0.0457 | 0.0542 | 0.2417-0.3612 (**fails**) | THE PUDDLE BAR: gas under `KNEE_M` 0.5 m over all gas |
| `t1_gas_cards` | `le` | 1300 | 1259, 1249, 1239 | 20 | 41 | 1051-1066 | five rims in eight, against `MaxCards` 1536 |
| `green_blob_share` | `le` | 0.065 | 0.0565, 0.0562, 0.0571 | 0.0009 | 0.0079 | 0.1530-0.1877 (**fails**) | THE CARPET BAR, new here |
| `green_rim_lift` | `ge` | 0.96 | 1.0005, 0.9839, 0.9845 | 0.0166 | 0.0239 | 0.9902-0.9933 (**clears it**) | new here, and it does not separate: see below |
| `gas_smoke_rise_ratio` | `ge` | 2.07 | 2.0752 x3 | 0 | 0.0052 | 2.0752 | the gas card's rise over the smoke card's |
| `gas_smoke_relief_ratio` | `ge` | 3.11 | 3.1125 x3 | 0 | 0.0025 | 3.1125 | the gas card's relief over the smoke card's |
| `o240_green_area_frac` | `ge` | 0.0075 | 0.0078, 0.0079, 0.0081 | 0.0003 | 0.0003 | the same | O240 is the unchanged drawing |
| `o240_large_cloud_count` | `ge` | 1 | 4, 3, 3 | 1 | 2 | 3, 3, 3 | carried |
| `o240_large_share` | `ge` | 0.71 | 0.7270, 0.7369, 0.7298 | 0.0099 | 0.0170 | 0.7273-0.7377 | carried |
| `pose_error_max` | `lt` | 0.50 | 0.0000 x3 | 0 | 0.5000 | the same | `GasBankStills.PoseBar` |

### Where this proof is weak

- **The two `cards_*` ratios are one measurement counted three times.** `GasBankCards` hand-packs one card per
  book from `now = 0` with a fixed hash at one pinned pose: it is deterministic, so the three runs read the same
  2.0752 and 3.1125 and their spread is zero. They also read the SAME numbers as the carpet build's captures, so
  this unit's `Which_Books` fix did not move them: the gas card the cards pass draws is unchanged in rise and in
  relief. Those two bars guard the gas card against collapsing under the smoke card and nothing more.
- **`green_height_frac` is no longer a bar**, only a printed number: this build reads 0.0191 / 0.0381 / 0.0393, a
  spread of 0.0202 that is bigger than its own smallest reading, so no floor above zero clears the spread rule.
  `green_rise_m` is the same quantity in metres and is barred at 0.50.
- **The rim bar does not separate the builds.** `green_rim_lift` was meant to fail the carpet build and does not.
  The rim is drawn as a faint wide halo (1.6x brightness at 0.55 alpha), not as a bright crown, so the top 30% of
  a green column — the cloud's own fading upper edge — is no brighter than the body below it, in either build.
  Raising the rim's share from three cells in eight to five and its alpha from 0.35 to 0.55 moved the key from
  0.9346-0.9971 to 0.9839-1.0005, still inside the carpet build's 0.9902-0.9933. Making this key mean something
  needs a drawing change nobody has approved (a bright crown over the cloud), not another turn of the knobs.

### The still still reads as scattered green puffs

Look at `run2/gas-t1.jpg`. The bank is no longer one 6-8 m band per cell, and `green_blob_share` proves the green
now breaks into separate clouds — but at zoom 30 the field reads as many small flat-topped green blobs with
ground between them, not as the standing cloud with a rim the owner asked for. **The owner's complaint is not
answered by this build.** What the numbers say is narrower: the close bank is no longer the carpet's single
connected sheet, it stands higher (0.90-1.29 m against 0.58-0.74 m), and less of it lies under the knee.

## Status

`python probe.py` exits 0, `OK`; `python probe.py --selftest` exits 0 (0.34311 m). These are regression bars set
from this build's own spread. The look is NOT signed off: the owner has not seen this build, and the still above
says he would repeat his complaint.
