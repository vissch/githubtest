# ability-fx-captures-r2-2-g2-g1-g2-g2-g2: the gas test rejects smoke and the puff card, 2026-10-21

**`python probe.py` clears, exit 0.** The build is unchanged (`CombatFx.Banks.cs`, commit `5c0c33620a`, wide
overlapping cards at low alpha); only the measure changed again. New here: one key, `gas_green_share`, and a check
that makes the probe REFUSE to pass unless the smoke card and the puff card fail that key.

## The owner's complaint

> Make the gas test reject smoke and the puff card. The new direction cone keeps 0.71 of the smoke card and 0.74 of
> the puff card, so `gas_paint_frac` passes on any translucent night card.

He is right, and `--signature` prints the numbers that say so (cone shares: gas 0.8000, smoke 0.7139, puff 0.7425).
The cone is an **amount floor** — "is there enough paint lifting roughly this way?" — not a test of what the paint
is. It stays, as that floor. What rejects a neutral card is new.

## The new key: `gas_green_share`

Chlorine's tint is **G over R**. For each pixel of the frame's plate-difference-and-brighter paint (the same mask
`gas_paint_stats` already builds: diff >= `T1_DIFF_BAR` 20 RGB, Rec.709 luma lift >= 2), take its own lift vector
`(dR, dG, dB)` and ask whether `(dG - dR) / |lift| >= GREEN_LEAN`. `gas_green_share` is the share of that paint
which does. One function, `green_share(vecs)`, measures it for both the frame and the isolation cards, so the two
can never be measured two different ways.

| | `gas_green_share` |
|---|---|
| run1 / run2 / run3 (t1 ticks 557 / 597 / 637) | 0.1535 / 0.1426 / 0.1495 |
| `cards-gas` (isolation still) | 0.2190 |
| `cards-smoke` | 0.0908 |
| `cards-puff` | 0.1076 |

Mean lift of the three cards' own paint: gas (26.1, 31.5, 34.7), smoke (25.4, 24.3, 26.9), puff (25.5, 23.8, 26.0)
— the gas card's lift rises in G and B over R; smoke's and puff's are near-neutral. That difference is what this
key reads.

## Why no other colour test does better

Measured this leg, all three dead ends:

- **The composite hue** is the night field's, not the gas's (g2-g2's README: median 156 PIL, blue). Dead already.
- **On the largest coherent patch** the three cards are nearly identical: gas (26.5, 30.6, 39.7), smoke
  (26.6, 29.9, 38.2), puff (25.6, 27.5, 33.0). Moving the measure onto the patch separates nothing.
- **Narrowing the cone or re-fitting `GAS_DIR` cannot work**: the runs' own mean lift direction
  (0.619, 0.654, 0.436) is FARTHER from `GAS_DIR` than smoke's is (cos 0.965 vs 0.994). Any tighter cone throws the
  build out before it throws smoke out.

Whole-frame green lean is the one axis measured where the runs sit above both cards. So: one key, and it is this one.

## The green lean: how 0.16 was picked

`GREEN_LEAN` scanned, with the bar rule applied at every t (a bar must clear the nearest run by the three runs'
own spread AND leave the worse card failing by that same spread, so the feasible band is
`max(cards) + spread  ..  min(runs) - spread`):

| t | run1 / run2 / run3 | `cards-smoke` | `cards-puff` | spread | feasible band | width |
|---|---|---|---|---|---|---|
| 0.10 | 0.2948 / 0.2712 / 0.3017 | 0.2050 | 0.2058 | 0.0305 | 0.2363-0.2407 | 0.0044 |
| 0.12 | 0.2485 / 0.2288 / 0.2511 | 0.1581 | 0.1677 | 0.0223 | 0.1900-0.2065 | 0.0165 |
| 0.14 | 0.1939 / 0.1790 / 0.1920 | 0.1203 | 0.1342 | 0.0149 | 0.1491-0.1641 | 0.0150 |
| **0.16** | **0.1535 / 0.1426 / 0.1495** | **0.0908** | **0.1076** | **0.0109** | **0.1185-0.1317** | **0.0132** |
| 0.18 | 0.1233 / 0.1145 / 0.1186 | 0.0686 | 0.0881 | 0.0088 | 0.0969-0.1057 | 0.0088 |
| 0.20 | 0.0931 / 0.0864 / 0.0865 | 0.0529 | 0.0711 | 0.0067 | 0.0778-0.0797 | 0.0019 |
| 0.25 | 0.0531 / 0.0487 / 0.0457 | 0.0313 | 0.0458 | 0.0074 | none | — |
| 0.30 | 0.0308 / 0.0303 / 0.0240 | 0.0204 | 0.0317 | 0.0068 | none | — |

t = 0.12 has the widest band in absolute terms (0.0165), but its spread is twice 0.16's, so a bar in its middle
clears only **1.37x** the spread on each side. At t = 0.16 the middle of the band gives **1.61x** above the runs
and **1.60x** below the puff card — the widest margins measured in the runs' own spread, which is the unit the bar
rule is written in. Hence `GREEN_LEAN = 0.16`.

## The rejection is part of the verdict

A bar alone could be set and the cards never looked at. `card_reject_check` reads `cards-smoke.jpg` and
`cards-puff.jpg` against `cards-plate.jpg` from `CARDS_DIR` (`../ability-fx-captures-r2-2-g2-g1/cards`) through the
same `green_share`, and `probe.py` cannot exit 0 unless both are below the bar by at least the runs' spread — or if
that folder has gone missing. What it prints:

```
card rejection (the cone passes any translucent night card; the green lean must refuse these two):
  cards-smoke  gas_green_share 0.0908  bar 0.1250  under by 0.0342 (3.14x spread) -> REJECTED
  cards-puff   gas_green_share 0.1076  bar 0.1250  under by 0.0174 (1.60x spread) -> REJECTED
```

## The zebra still passes this key, honestly

`python probe.py --against ../ability-fx-captures-r2-2-g2` reads `gas_green_share` **0.2446 / 0.2446 / 0.2447** on
the zebra — it PASSES this key, and higher than the build does. The zebra was flat yellow-green paint, so a
green-lean test cannot see what is wrong with it. That is fine and it is not this key's job:
`bank_paint_share` (zebra 0.0782, bar 0.12) and `bank_patch_count` (zebra 40, bar 25) are the keys that reject the
zebra, on patch coherence, and `--against` still exits 1.

## The bars

| key | cmp | bar | the three runs | spread | margin | why |
|---|---|---|---|---|---|---|
| `bank_low_share` | `ge` | 0.07 | 0.4371 / 0.2584 / 0.2853 | 0.1787 | 0.1884 | unchanged from g2 |
| `bank_height_frac` | `le` | 0.27 | 0.0799 / 0.1711 / 0.1608 | 0.0912 | 0.0989 | unchanged from g2 |
| `o240_area_frac` | `ge` | 0.010 | 0.0130 / 0.0150 / 0.0153 | 0.0023 | 0.0030 | unchanged from g2 |
| `pose_error_max` | `lt` | 0.50 | 0.0000 x3 | 0.0000 | 0.5000 | `GasBankStills.PoseBar` |
| `gas_paint_frac` | `ge` | 0.0130 | 0.0146 / 0.0151 / 0.0153 | 0.0007 | 0.0016 | unchanged from g2-g2; the amount floor, 2.3x the spread |
| `gas_green_share` | `ge` | 0.125 | 0.1535 / 0.1426 / 0.1495 | 0.0109 | 0.0176 | NEW: 1.61x the spread; smoke fails by 3.14x, puff by 1.60x |
| `bank_paint_share` | `ge` | 0.12 | 0.1855 / 0.1622 / 0.1936 | 0.0314 | 0.0422 | unchanged from g2; zebra 0.0782 fails by 0.0378 |
| `bank_patch_count` | `le` | 25 | 10 / 13 / 7 | 6 | 12 | unchanged from g2; zebra 40 fails by 15 |

## No reshoot

`git diff --stat d1480a099b HEAD -- trench-warfare-3d/Assets` is empty: the build has not moved, so
`run1`/`run2`/`run3` here are copied byte-for-byte from `../ability-fx-captures-r2-2-g2-g1-g2-g2/`, still t1
`sim_tick` 557 / 597 / 637, still twelve distinct hashes, and `one_frame_fault` still refuses two runs on one
frame. No `cards/` here: the isolation stills did not change and stay in
`../ability-fx-captures-r2-2-g2-g1/cards/` (same pose as `gas-t1`: cam 110.24191 / 33.01822 / 96.48563, pitch 25,
yaw 310, zoom 30). The in-folder look report is therefore skipped (`no cards/`) — the card REJECTION check reads
that folder directly and is not skippable.

## The sheet, as a number

`bank_width_m` and `bank_aspect`, unbarred, carried over unchanged as the baseline for whatever build next turns
the sheet into a puff:

| run | `bank_width_m` | `bank_rise_m` | `bank_aspect` |
|---|---|---|---|
| run1 (tick 557) | 9.071 | 1.845 | 4.918 |
| run2 (tick 597) | 9.223 | 3.443 | 2.679 |
| run3 (tick 637) | 9.351 | 3.094 | 3.022 |

The width barely moves (9.07-9.35 m) while the rise triples as the gas ages (1.8-3.4 m): a sheet across the trench,
not a puff.

## Out of scope

Turning the sheet into a puff (a `CombatFx.Banks.cs` change), the `concepts/` brief images, and rejecting the zebra
with this key.
