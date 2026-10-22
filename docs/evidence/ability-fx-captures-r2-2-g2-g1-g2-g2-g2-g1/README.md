# ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1: one low chlorine bank, not a sheet of stripes, 2026-10-22

**`python probe.py` clears, exit 0.** This is the first folder in the chain where the BUILD moved: every leg since
`5c0c33620a` changed only the measure. `CombatFx.Banks.cs` is at `3d7aa478ea` here, and `run1`/`run2`/`run3` are
three fresh captures of it (t1 `sim_tick` 557 / 597 / 637, twelve distinct hashes, `pose_error_m` 0 everywhere,
`books_ready` true, `calls_taken` 1 at T1 and 5 at O240). Every bar in `BAR_VALUES` is re-set from them.

## The owner's complaint

> Paint one low chlorine bank a player can see at zoom 30. The three gas-t1 stills are the same sheet of pale green
> stripes, and the build did not move.

Both halves are answered: the build moved, and the stripes are gone by eye (`run1/gas-t1.jpg` against
`../ability-fx-captures-r2-2-g2-g1-g2-g2-g2/run1/gas-t1.jpg`, same pose, same tick). The old frame reads as four or
five horizontal bands with dark field between them; the new one reads as one continuous green mass with a brighter
core. It is still a LOW bank — that is the point, and a landed guard keeps it there.

## The build change (`3d7aa478ea`)

| | before | after | why |
|---|---|---|---|
| `GasBankWidth` | `(9 + 4t) * ... * step`, 8-14.3 m | `(13.5 + 4t) * ... * step`, 12-19.3 m | a card spans three 4 m field cells, so neighbours overlap three to four deep and no single card's silhouette is the shape you see |
| `GasBankThin` / `GasBankFull` | 0.14 / 0.5 | 0.19 / 0.62 | "pale" is the alpha: three stacked cards now reach 0.945, not 0.875 |
| roll | +-7 deg | +-12 deg | no two card edges line up, which is what made the bands |
| `GasBankSquash` | 0.36 | 0.27 | the man rule: the widest card (19.3 m) tops out at 1.73 m, under `HEAD_M` 1.8 |
| guard | — | the cards must overlap into one mass at 12 m | `BurstRecipeTests.TheGasBankCardsOverlapIntoOneMass`, the 4 m grid x 3 cells |

The height was NOT raised. The landed guard
`BurstRecipeTests.TheGasBankLiesOnTheGround_NotOverAMansHead` caps `GasBankWidth(1,1,1) * GasBankSquash / 3` under
1.8 m, so squash cannot rise at these widths — and "one low bank a player can see" is coherence and contrast, not
height. The hue is untouched: `GAS_DIR`/`GAS_COS` were fitted from `cards-gas.jpg`, and another lane already has a
hue brief open with the owner.

## The unanswered brief

`G:/My Drive/TW3D-pipeline/decisions/2026-10-21-gas-bank-turn-the-sheet-into-a-puff/brief.json` is still
`state: open` with no answer file. Nobody can be asked inside a relay leg, so CLAUDE.md's "nobody to ask" rule
applies: the default taken is the **buildable half of option A** — one mass instead of stripes, less pale, man rule
kept — recorded in `docs/reference/decisions.md` at `4101179359`. **This lane does not land on it.**

## The bars

Rule, unchanged: a bar clears the nearest run by at least the three runs' own spread; for `bank_paint_share` and
`bank_patch_count` it must also leave the zebra (`--against ../ability-fx-captures-r2-2-g2`) failing by more than
that spread.

| key | cmp | bar | run1 / run2 / run3 | spread | margin | why |
|---|---|---|---|---|---|---|
| `bank_low_share` | `ge` | 0.15 | 0.3743 / 0.2661 / 0.3238 | 0.1082 | 0.1161 | re-set; the roof the rule allows is 0.1579 |
| `bank_height_frac` | `le` | 0.31 | 0.1118 / 0.2096 / 0.1375 | 0.0978 | 0.1004 | re-set; the floor the rule allows is 0.3074 |
| `o240_area_frac` | `ge` | 0.014 | 0.0171 / 0.0193 / 0.0195 | 0.0024 | 0.0031 | re-set, up from 0.010: the wider cards cover more of the overview |
| `pose_error_max` | `lt` | 0.50 | 0.0000 x3 | 0.0000 | 0.5000 | `GasBankStills.PoseBar` |
| `gas_paint_frac` | `ge` | 0.0135 | 0.0154 / 0.0156 / 0.0148 | 0.0008 | 0.0013 | re-set, up from 0.0130; 1.6x the spread |
| `gas_green_share` | `ge` | 0.183 | 0.2296 / 0.2131 / 0.2357 | 0.0226 | 0.0301 | re-set, up from 0.125. Three sides at once (below) |
| `bank_paint_share` | `ge` | 0.15 | 0.2037 / 0.1886 / 0.2152 | 0.0266 | 0.0386 | re-set, up from 0.12; zebra 0.0782 fails by 0.0718 |
| `bank_patch_count` | `le` | 26 | 15 / 18 / 11 | 7 | 8 | re-set, UP from 25 — see "the one key that went the wrong way" |
| `bank_aspect` | — | none | 5.315 / 3.014 / 4.485 | 2.301 | — | no feasible band: see below |

`gas_green_share` 0.183 is the only key barred against three sides at once. The runs allow at most 0.1905; the
isolation cards need at least `0.1076 + 0.0226 = 0.1302`; and the OLD pale sheet must fail by a spread too, which
needs at least `0.1535 + 0.0226 = 0.1761`. The band is 0.1761-0.1905 and 0.183 is its middle: the runs clear it by
1.33x their spread, `cards-smoke` fails by 4.08x, `cards-puff` by 3.34x, the old sheet by 1.35x.

```
card rejection (the cone passes any translucent night card; the green lean must refuse these two):
  cards-smoke  gas_green_share 0.0908  bar 0.1830  under by 0.0922 (4.08x spread) -> REJECTED
  cards-puff   gas_green_share 0.1076  bar 0.1830  under by 0.0754 (3.34x spread) -> REJECTED
```

`GREEN_LEAN` stays 0.16: it is a property of the measure, not of the build, and the new build only pushes the runs
farther above the cards.

## The one key that went the wrong way

`bank_patch_count` is the stripes key, and the plan expected it to fall well under the old 10 / 13 / 7. It did the
opposite: **15 / 18 / 11**, and the bar had to go UP from 25 to 26. That is reported, not hidden.

Why it rose: the key counts paint patches of 200 px or more, and the higher alpha lifted far more of the frame over
`T1_DIFF_BAR`. `mask_px_raw` goes 46,702 / 50,755 / 47,337 -> 73,922 / 77,341 / 71,307 — half as much paint again.
Thin fringes that used to sit under the diff bar now clear it and count as patches of their own. `bank_patch_count`
measures *how much paint is visible at all* as much as it measures coherence, so on a build that paints more it
cannot be read as "more stripes". The coherence reading that does hold is `bank_paint_share`, the share of paint in
the ONE largest patch: **0.1855 / 0.1622 / 0.1936 -> 0.2037 / 0.1886 / 0.2152**, up on every run. One mass took a
larger share of a larger total.

The honest limit: no number in this probe says "not stripes" on its own. The eye check on `run1/gas-t1.jpg` is part
of this folder's verdict, and it was done before the commit.

## Before and after

| key | g2-g2-g2 (old sheet) | g1 (this build) |
|---|---|---|
| `bank_patch_count` | 10 / 13 / 7 | 15 / 18 / 11 |
| `bank_paint_share` | 0.1855 / 0.1622 / 0.1936 | 0.2037 / 0.1886 / 0.2152 |
| `gas_paint_frac` | 0.0146 / 0.0151 / 0.0153 | 0.0154 / 0.0156 / 0.0148 |
| `gas_green_share` | 0.1535 / 0.1426 / 0.1495 | 0.2296 / 0.2131 / 0.2357 |
| `bank_width_m` | 9.071 / 9.223 / 9.351 | 13.835 / 13.415 / 14.464 |
| `bank_aspect` | 4.918 / 2.679 / 3.022 | 5.315 / 3.014 / 4.485 |
| `bank_rise_m` | 1.845 / 3.443 / 3.094 | 2.603 / 4.450 / 3.225 |

The bank is 4.3-5.1 m wider and holds 1.4-1.6x the green lean: wider and less pale, as intended.

`python probe.py --against ../ability-fx-captures-r2-2-g2-g1-g2-g2-g2` exits 1, and it is a real failure, not the
by-design one: the old sheet reads `gas_green_share` 0.1535 / 0.1426 / 0.1495 against the 0.183 bar, under it on
every run.

## `bank_aspect` stays unbarred

A `le` bar on 5.315 / 3.014 / 4.485 (spread 2.301) must sit at 7.616 or higher to clear the nearest run by a
spread. That is nowhere near "under the old build's 2.68-4.92", so no bar on this key could tell the two builds
apart. It is printed per run, unbarred, as the baseline for whatever build next changes the bank's shape. The
run-to-run swing (3.0 to 5.3) is the gas ageing between ticks 557 and 637, not capture noise.

## No `cards/` here

The isolation stills did not change and stay in `../ability-fx-captures-r2-2-g2-g1/cards/` (same pose as `gas-t1`:
cam 110.24191 / 33.01822 / 96.48563, pitch 25, yaw 310, zoom 30). The in-folder look report is skipped
(`no cards/`); the card REJECTION check reads that folder directly and is not skippable.

## Out of scope

The gas hue and its colour constants, reshooting `cards/`, `DrawSmokeBank`, the `concepts/` brief images, and
landing this lane (the puff brief is unanswered).
