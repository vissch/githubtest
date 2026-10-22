# ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g1: the gaps are closed, one bank, 2026-10-22

**`python probe.py` clears, exit 0.** The build is at `71ce0ccd73`; `run1`/`run2`/`run3` are three fresh captures
of it (t1 `sim_tick` 557 / 597 / 637, `books_ready` true, `pose_error_m` 0 everywhere, `calls_taken` 1 at T1 and 5
at O240, `gas_cards` 774 / 743 / 709, `draw_calls` 133 / 124 / 120 at T1 and 118-119 at O240 - the cards cost
nothing new). Every bar in `BAR_VALUES` is re-set from them. **The measure is untouched**: the probe is a copy of
the `-g1` one with new bar values only, because the owner asked for the floor, not a new measure.

## The owner's complaint

> At zoom 30 the three gas-t1 stills are still four or five horizontal yellow-green bands with dark ground between
> them. The plate is bare mud, so those bands are the gas. `bank_paint_share` is 0.2037, 0.1886 and 0.2152, and the
> 0.15 floor sits under the old striped sheet's worst run of 0.1622, so that sheet still passes. Close the gaps
> until the frame is one low bank, and put the floor above both this build and the old sheet.

Both halves are answered.

- **The gaps are closed.** `bank_paint_share` goes 0.2037 / 0.1886 / 0.2152 -> **0.7488 / 0.7575 / 0.7872**: three
  quarters of all the paint in the frame now sits in ONE patch. By eye on `run1/gas-t1.jpg` the frame is one
  continuous green mass over the whole field, with no mud stripe between rows.
- **The floor is above both builds.** `bank_paint_share` is barred at **0.70**. The g1 build (0.2037 / 0.1886 /
  0.2152) and the old striped sheet (0.1855 / 0.1622 / 0.1936) both fail it on every run, by over 0.48 - twelve
  times the new runs' own spread.

## Why the bands were there (`71ce0ccd73`)

The bands are the field's own 4 m rows. `GasBankSquash` was a SHARE of the card's width, so a thin cell's narrow
card painted a short band: 1.09 m of gas against a 4 m row pitch. At the T1 pose (pitch 25 from `GasBankStills`,
4 m from `MapData.FieldCellSize`) a row's band covers `band * cos 25` of the `4 * sin 25` = 1.69 m the next row
sits away in depth - 1.09 m covered 59 % of it, so two fifths of every gap stayed bare mud.

| | before | after | why |
|---|---|---|---|
| band height | `width * GasBankSquash` (0.27), 1.09-1.73 m | `GasBankBandM` 1.72 m for every card (`GasBankSquashFor(width)`) | 92 % of the row pitch, still under a 1.8 m man's head |
| card height | `b.Ground - 0.35 + (h1-0.5)*0.5` | `b.Ground - 0.08` | the sink and the +-0.25 m jitter threw away 0.6 m of band and pulled the rows apart |
| `full` (what counts as thick) | 14 | 8 | most cards now draw at full width and alpha |
| sideways jitter | 2.4 m | 1.2 m | a row stays a row |
| guard | - | `BurstRecipeTests.TheGasBankBandsMeetInDepthAtTheT1Pose`: `band * cos 25 >= 0.90 * 4 * sin 25` | the rows must meet at the play pose |

The stand floor is untouched, so the card count and the draw calls do not move.

## The bars

Rule, unchanged: a bar clears the nearest run by at least the three runs' own spread; `bank_paint_share` and
`bank_patch_count` must also leave the zebra (`--against ../ability-fx-captures-r2-2-g2`) failing by more than that
spread, and `bank_paint_share` carries the owner's extra condition (above both older builds).

| key | cmp | bar | run1 / run2 / run3 | spread | margin | why |
|---|---|---|---|---|---|---|
| `bank_low_share` | `ge` | 0.06 | 0.0768 / 0.0728 / 0.0814 | 0.0086 | 0.0128 | re-set DOWN from 0.15: see "the keys that moved the wrong way" |
| `bank_height_frac` | `le` | 0.34 | 0.2946 / 0.3132 / 0.2985 | 0.0186 | 0.0268 | re-set UP from 0.31, same reason |
| `o240_area_frac` | `ge` | 0.013 | 0.0164 / 0.0186 / 0.0191 | 0.0027 | 0.0034 | re-set down from 0.014: run1's overview is a touch thinner |
| `pose_error_max` | `lt` | 0.50 | 0.0000 x3 | 0.0000 | 0.5000 | `GasBankStills.PoseBar` |
| `gas_paint_frac` | `ge` | 0.016 | 0.0195 / 0.0188 / 0.0180 | 0.0015 | 0.0020 | re-set UP from 0.0135: the full band paints more |
| `gas_green_share` | `ge` | 0.37 | 0.4039 / 0.3911 / 0.4013 | 0.0128 | 0.0211 | re-set UP from 0.183. Three sides at once (below) |
| `bank_paint_share` | `ge` | **0.70** | 0.7488 / 0.7575 / 0.7872 | 0.0384 | 0.0488 | the owner's floor; zebra 0.0782, old sheet 0.1936, g1 0.2152 all fail |
| `bank_patch_count` | `le` | 27 | 21 / 21 / 15 | 6 | 6 | re-set up from 26 - the key that rose again |
| `bank_aspect` | - | none | 3.931 / 3.432 / 3.839 | 0.499 | - | still unbarred, as in `-g1`: printed per run as the baseline |

`gas_green_share` 0.37 clears three sides at once: the runs allow at most 0.3783; the isolation cards need at
least `0.1076 + 0.0128 = 0.1204`; the old pale sheet needs at least `0.1535 + 0.0128 = 0.1663`. The runs clear
0.37 by 1.65x their spread, `cards-smoke` fails by 21.8x and `cards-puff` by 20.5x.

```
card rejection (the cone passes any translucent night card; the green lean must refuse these two):
  cards-smoke  gas_green_share 0.0908  bar 0.3700  under by 0.2792 (21.81x spread) -> REJECTED
  cards-puff   gas_green_share 0.1076  bar 0.3700  under by 0.2624 (20.50x spread) -> REJECTED
```

`GREEN_LEAN`, `GAS_DIR`, `GAS_COS` and `CARDS_DIR` are unchanged.

## The keys that moved the wrong way

Reported, not hidden. Three keys read worse than on `-g1`:

- **`bank_patch_count` rose again**, 15 / 18 / 11 -> **21 / 21 / 15**, and the bar went up from 26 to 27. Same
  cause as last time, written up in `-g1`: the key counts paint patches of 200 px or more, and this build paints
  far more of the frame (`mask_px_raw` 73,922 / 77,341 / 71,307 -> **197,867 / 192,353 / 184,315**, two and a half
  times as much). Fringes that used to sit under `T1_DIFF_BAR` 20 now clear it and count as patches of their own.
  On a build that paints more, this key cannot be read as "more stripes"; `bank_paint_share` is the coherence
  reading that holds, and it tripled.
- **`bank_low_share` fell**, 0.3743 / 0.2661 / 0.3238 -> **0.0768 / 0.0728 / 0.0814**, and **`bank_height_frac`
  rose**, 0.1118 / 0.2096 / 0.1375 -> **0.2946 / 0.3132 / 0.2985**. Both bars were re-set from the new runs
  (0.15 -> 0.06 and 0.31 -> 0.34). These two measure the ONE blob the mask finds: with the gaps closed, that blob
  is now the whole field-wide bank instead of a single row, so it reaches far up the frame and a smaller share of
  it sits in the bottom band. The bank itself did NOT get taller: `GasBankBandM` is 1.72 m, under `HEAD_M` 1.8,
  and `BurstRecipeTests.TheGasBankLiesOnTheGround_NotOverAMansHead` holds that in metres, which no screen-share
  key can. `bank_rise_m` 2.603 / 4.450 / 3.225 -> 6.862 / 7.461 / 7.145 reads the same way: it is the bank running
  away from the camera in depth, not its height.

## Before and after

| key | g2-g2-g2 (old sheet) | g1 | g1-g1 (this build) |
|---|---|---|---|
| `bank_paint_share` | 0.1855 / 0.1622 / 0.1936 | 0.2037 / 0.1886 / 0.2152 | **0.7488 / 0.7575 / 0.7872** |
| `bank_patch_count` | 10 / 13 / 7 | 15 / 18 / 11 | 21 / 21 / 15 |
| `gas_paint_frac` | 0.0146 / 0.0151 / 0.0153 | 0.0154 / 0.0156 / 0.0148 | 0.0195 / 0.0188 / 0.0180 |
| `gas_green_share` | 0.1535 / 0.1426 / 0.1495 | 0.2296 / 0.2131 / 0.2357 | 0.4039 / 0.3911 / 0.4013 |
| `bank_low_share` | 0.4371 / 0.2584 / 0.2853 | 0.3743 / 0.2661 / 0.3238 | 0.0768 / 0.0728 / 0.0814 |
| `bank_width_m` | 9.071 / 9.223 / 9.351 | 13.835 / 13.415 / 14.464 | 26.977 / 25.607 / 27.427 |
| `bank_aspect` | 4.918 / 2.679 / 3.022 | 5.315 / 3.014 / 4.485 | 3.931 / 3.432 / 3.839 |

## The two `--against` runs

Both exit 1, both for the owner's reason, `bank_paint_share` under the new 0.70 floor on all six runs:

```
--against ../ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1   bank_paint_share 0.2037 / 0.1886 / 0.2152   -> exit 1
--against ../ability-fx-captures-r2-2-g2-g1-g2-g2-g2      bank_paint_share 0.1855 / 0.1622 / 0.1936   -> exit 1
```

## The eye check

`run1/gas-t1.jpg`, the part of the verdict no number gives: one green mass lying over the whole field, bright at
its core, thinning at the edges, with the wreck, the duckboards and the stakes reading through it. No horizontal
mud stripe anywhere between the rows. Done before the commit.

## The unanswered brief

`2026-10-21-gas-bank-turn-the-sheet-into-a-puff` is still open with no answer (`4101179359` in
`docs/reference/decisions.md`). **This lane does not land.** Push only.

## No `cards/` here

The isolation stills did not change; `CARDS_DIR` still points at `../ability-fx-captures-r2-2-g2-g1/cards/`. The
in-folder look report is skipped (`no cards/`); the card REJECTION check reads that folder directly and is not
skippable.

## Out of scope

The gas hue and its colour constants, reshooting `cards/`, `DrawSmokeBank`, lowering the stand floor, and landing
the lane.
