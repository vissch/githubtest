# ability-fx-captures-r2-3-g2-g2-g1-g1-g1-g1 — the gaps closed, the dome bar still short

The owner's words: *"At zoom 30 the bank is still separate horizontal green pads with mud between the rows, and
from the side those pads are stacked in three or four rows. Close the gaps until one cloud stands, with a rim
lower than the middle. Put `side_dome_ratio`'s floor above 1.6646, this build's best, and above 1.0 so a flat pad
fails."*

The gaps are closed and the bank stands half again higher. **The dome bar is NOT raised**, because this capture
does not support a floor over 1.6646 and a bar is never fitted to a run. The measured reason is below.

```bash
TW_STILLS_DIR=<dir> unity.exe test . --mode EditMode --filter TW.Tests.GasBankStills --timeout 1800  # the gas pass
TW_GAS_PLATE=1 TW_STILLS_DIR=<dir> ...                                                               # the plate pass
python docs/evidence/ability-fx-captures-r2-3-g2-g2-g1-g1-g1-g1/probe.py --pack <dir>
```

## Why there was mud between the rows, measured off the lens

Each pad was one card's ink. The `GasBank` sheet fills the bottom third of an upright card, so a card reads as a
slab with a hard bottom edge, and cells stand every `MapData.FieldCellSize` = 4 m. At T1's pose in
`GasBankStills` (pitch 25) a card's ink must be at least `4 * tan(25) = 1.87 m` tall for one row's ink to reach
the next row's base. A typical cell drew a 6 m card squashed to 0.75, so its ink topped out at
`6 * 0.75 / 3 = 1.50 m` — **0.37 m short of the lens**, and that shortfall is the mud stripe, one per row.

## What changed in the look

All four changes are in `CombatFx.Banks.cs`; the tests are in `BurstRecipeTests.cs`.

| Change | Numbers |
|---|---|
| **The bed stands tall enough to bridge a row.** `GasBankSquash` 0.75 -> **1.20**, so a thin 6 m card's ink tops out at 2.40 m: a 28 % margin over the 1.87 m T1 needs. `GasBankCore` 1.15 -> **1.45**, keeping the old 0.25 lead for the thick heart. | squash 1.20, core 1.45 |
| **The rim is a fraction of the bed, not of the card's width.** `DrawGasBank` takes `float bed = GasBedHeight(cw, b.Thick)` and draws `b.Edge ? bed * GasBankRim : bed`, with `GasBankRim` 0.95 -> **0.45**. The lip's ink is then always 0.45 of the bed's whatever the bed does — "a rim lower than the middle" as one number instead of two that drift apart. The 0.85 width narrowing and the hash roll are unchanged. | rim 0.45 of the bed |
| **Only the interior is crowned.** `GasBlock` gains `Edge`, and the crown gate goes from `blk.Thick < 0.5f` to `blk.Count < 3`, `blk.Edge`, or `blk.Thick < 0.25f`. The old gate (`Thick < 0.5`, so 7 of 14) made crowns rare, so the middle was no higher than the edge. | thick floor 0.25, no rim block |
| **The crown sits ON the bed with its ink touching.** `GasCrownLift` 0.5 -> **0.9** of the bed's drawn top: at `thick = 1, step = 1` the lift is 3.04 m and `GasCrownTop` reaches 5.44 m. 0.9 and not 1.0, so the crown's ink overlaps the bed's and the two read as one body. | lift 0.9 |
| The recipe tests follow the shapes: `TheGasBankLiesOnTheGround_NotOverAMansHead` becomes **`TheGasBankBedBridgesARow`** and asserts the bed against `4 * tan(25)` — the expected value comes from the capture pose, not from the code's own arithmetic. `TheGasBankStandsAsAVolume` reads the rim off the bed and adds `rim < bed * 0.5`. | |

The `FlipbookFx.cs` `GasBank` sheet row is **untouched**: `texture_var_drop` stayed inside its band without it.

## This build against `...-g1-g1-g1`, seed by seed

| reading | `...-g1-g1-g1` | this build |
|---|---|---|
| `texture_var_drop` (band 0.40 .. 0.70) | 0.5068 / 0.6472 / 0.4302 | 0.5196 / 0.6232 / 0.4340 |
| `side_crown_m` | 2.561 / 2.496 / 2.640 | **3.880 / 3.688 / 3.808** |
| `gas_side_height_m` (reported) | 2.6-2.8 | 3.268 / 2.699 / 3.046 |
| `side_dome_ratio` | **1.6646** / 1.2726 / 1.4459 | 1.8870 / 1.1991 / **1.1945** |
| `side_face_px` | 78606 / 34245 / 109479 | 123938 / 68453 / 164983 |
| `t1_changed_frac` | 0.2111 worst | 0.2921 worst |
| `side_outside_frac` (pack) | 0.2539 | **0.2160** |
| `side_face_spread` (pack) | 3.1969 | **2.4102** |

**The bank stands, and it hides more of the ground without flattening.** `side_crown_m` rises about 48 % on every
seed (worst 2.496 -> 3.688 m) — the first round in which the crown beat the old flat pad, which is what the
previous README said was still missing. `side_face_px` doubles on the worst seed (34245 -> 68453): there is far
more upright face on screen. `texture_var_drop` barely moves (worst 0.6232 against the 0.70 ceiling, sparsest
0.4340 against the 0.40 floor), so the taller ink hides the rows without turning the bank into a sheet.
`side_face_spread` falls 3.1969 -> 2.4102: the three seeds' banks are less unlike each other than before.

## The dome bar the owner asked for, and why this capture cannot carry it

The owner asked for `side_dome_ratio` floored over **1.6646** and over **1.0**. This capture reads
**1.8870 / 1.1991 / 1.1945**. The lane's rule is

```
side_dome_ratio >= floor((min - max(0.10, max - min)) * 100) / 100
                =  floor((1.1945 - 0.6925) * 100) / 100  =  0.50
```

which would **lower** the bar, so it is not taken. The bar is left at **0.88**, where `...-g1-g1-g1` set it, and
`side_crown_m` is raised from its own rule instead: `floor(0.8 * 3.688 * 10) / 10` = **2.9 m**, up from 1.9.
`TEX_CEIL` is held at 0.70.

**The plan's own answer to a short read was captured, and made it worse.** Raise the crown lift 0.9 -> 1.0 and
drop `GasBankRim` 0.45 -> 0.35, capture three seeds again:

| | lift 0.9, rim 0.45 (this build) | lift 1.0, rim 0.35 |
|---|---|---|
| `side_crown_m` | 3.880 / 3.688 / 3.808 | 4.122 / 3.953 / 4.093 |
| `side_dome_ratio` | 1.8870 / 1.1991 / **1.1945** | 1.8082 / 1.2102 / **1.1739** |
| `texture_var_drop` | 0.5196 / 0.6232 / 0.4340 | 0.5086 / 0.6306 / 0.4418 |

The crown goes higher and the ratio does not follow. So the lever is not the crown's height, and the tree was put
back to 0.9 / 0.45 (the plan's numbers, and the ones that keep the crown's ink overlapping the bed's).

**What the numbers say the lever is.** `side_dome_ratio` is the median net silhouette top of the middle half of
the blob's columns over the median of the outer quarters. Seed 0 — the biggest cloud — rose 1.6646 -> 1.8870.
Seeds 1 and 2 — the smaller clouds — FELL (1.2726 -> 1.1991, 1.4459 -> 1.1945). That split is what the new crown
gate predicts: a block is 2 x 2 cells, 8 m across, and it is crowned only when **none** of its four cells is a
rim cell. A gas cloud spreads about 10 m, which is about five cells, so on a small cloud nearly every cell is a
rim cell and **nearly no block is crowned at all**. The gate that was meant to put the dome in took the crown
away from two seeds of three.

Two things follow, and neither is a bar to widen:

1. **The crown gate is too strict for a cloud this size.** `blk.Edge` is the whole of it; asking for a block with
   at least two non-rim cells, or crowning a rim block at a lower lift, would put the crown back on the small
   seeds. That is a look change and a capture, not a number.
2. **The reading is diluted by depth.** The side band grazes the bank at pitch 8, so a column of pixels at the
   left or right END of the blob still has interior cells behind it along the ray: the "outer quarter" is not the
   rim alone, and the rim knob therefore has weak leverage on the ratio. A ratio over 1.67 may be asking for a
   bank whose edge is nearly flat on screen, which is not the same thing as a rim lower than the middle in world
   space. If the owner wants the bar that high, the honest way is a reading that measures the rim where the rim
   is, not where the silhouette ends.

## Every bar, this capture

```
t1_changed_frac    want >= 0.08    worst 0.2921    OK
texture_var_drop   want >= 0.4     worst 0.434     OK
o240_hue_dist_deg  want >= 108.0   worst 133.83    OK
side_face_px       want >= 30000   worst 68453     OK
side_crown_m       want >= 2.9     worst 3.688     OK
side_dome_ratio    want >= 0.88    worst 1.1945    OK
texture_var_drop   want <= 0.7     worst 0.6232    OK
side_outside_frac  want <= 0.4     read  0.216     OK
side_face_spread   want <= 3.55    read  2.4102    OK
```

## What is still not answered

- **`side_dome_ratio` does not clear 1.6646, and the goal asked it to.** Stated above with both captures, not
  papered over. The next round's lever is the crown gate, not the crown's height.
- **The owner has not seen these stills.** The look was named in his goal, so it was built; the captures go to
  him with `briefs.py` afterwards. `gas-t1-s0.jpg` and its neighbours are the ones to look at for "mud between
  the rows", `gas-side-s0.jpg` and its neighbours for the stacked rows.
- The plan also asked for two `--selftest` asserts on the dome bar (over 1.6646, and over the flat pad's 1.000).
  They are **not** written, because they would be red: an assert kept green by a bar this capture cannot support
  would be the opposite of the guard the owner asked for. The flat-pad case is still measured (`pad_r` 1.000),
  and it is the number the next round has to beat at its worst seed.
