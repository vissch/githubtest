# ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g2: the gas test rejects the zebra too, 2026-10-22

**`python probe.py` clears, exit 0.** The build is unchanged (same `CombatFx.Banks.cs`, no reshoot — see "No
reshoot"); only the measure changed again. `gas_green_share` becomes a WINDOW (floor and ceiling), not a floor, so
it now rejects the zebra as well as the smoke and puff cards.

## The owner's complaint

> Make `gas_green_share` reject the zebra as well as the smoke card and the puff card. The zebra reads 0.2446 and
> passes the bar of 0.125 that sits between the stripes (0.1426) and the puff card (0.1076).

He is right: a single `ge` bar can only refuse one side. The smoke and puff cards read BELOW the build's own
paint and the zebra reads ABOVE it, so no one-sided bar can refuse both.

## The window

`BAR_VALUES['gas_green_share'] = (0.125, 0.2318)`. The floor (0.125) is unchanged from the predecessor — it still
rejects the cards. The ceiling is new.

## The ceiling

Its source is the gas isolation card `cards-gas` (0.2190): nothing composited from that card at low alpha over the
night field can lean greener than the card itself, so paint greener than `cards-gas` is not chlorine — it is flat
paint, exactly what the zebra was.

The runs' own spread is 0.0109 (0.1535 - 0.1426). The ceiling must leave `cards-gas` (0.2190) inside by at least
that spread, so `<= 0.2299` is not enough room on its own — the band is `cards-gas + spread` on the low side and
`min(zebra) - spread` on the high side: `0.2299` to `0.2337`. 0.2318 is its balanced middle: 1.17x the spread
above the gas card and 1.17x below the zebra, and 7.2x above the highest run. `GREEN_LEAN` stays 0.16 — both the
floor band (0.1185-0.1317) and this ceiling band are feasible at it, so there was nothing to rescan.

## One mask, three measurements

`paint_mask(img, plate, bar)` is now the one place the PAINT mask (plate difference >= bar, brighter than the
plate) is built. `gas_paint_stats` (the frame), `card_paint_vectors` (the isolation cards) and the new
`frame_green_share` (the zebra's own runs, gas-t1 against gas-t1-plate) all build their mask through it, so the
frame, the cards and the zebra are measured exactly one way.

## Every still measured by this key

| still | `gas_green_share` | window 0.1250-0.2318 |
|---|---|---|
| run1 / run2 / run3 (this build) | 0.1535 / 0.1426 / 0.1495 | inside |
| `cards-gas` (isolation still) | 0.2190 | inside — required ACCEPT |
| `cards-smoke` | 0.0908 | REJECTED (below floor) |
| `cards-puff` | 0.1076 | REJECTED (below floor) |
| zebra run1 / run2 / run3 | 0.2446 / 0.2446 / 0.2447 | REJECTED (above ceiling) |

## The rejection is part of the verdict

`card_reject_check` now also requires `cards-gas` to sit INSIDE the window by at least the runs' spread, as a
required ACCEPT — the ceiling's source, so a look change that moves the gas card's own paint breaks this probe
rather than silently redefining the ceiling. `zebra_reject_check` reads the zebra's three runs from `ZEBRA_DIR`
(`../ability-fx-captures-r2-2-g2`) through `frame_green_share` and FAILS unless every one sits above the ceiling
by at least that spread — it is a hard dependency: missing runs FAIL loudly, they are never skipped. What it
prints:

```
card rejection (the cone passes any translucent night card; the green lean must refuse these two):
  cards-smoke  gas_green_share 0.0908  floor 0.1250  under by 0.0342 (3.14x spread) -> REJECTED
  cards-puff   gas_green_share 0.1076  floor 0.1250  under by 0.0174 (1.60x spread) -> REJECTED
  cards-gas    gas_green_share 0.2190  window 0.1250-0.2318  inside by 0.0128 (1.17x spread) -> ACCEPTED
zebra rejection (flat yellow-green paint leans green; the ceiling must refuse it):
  run1   gas_green_share 0.2446  ceiling 0.2318  over by 0.0128 (1.17x spread) -> REJECTED
  run2   gas_green_share 0.2446  ceiling 0.2318  over by 0.0128 (1.17x spread) -> REJECTED
  run3   gas_green_share 0.2447  ceiling 0.2318  over by 0.0129 (1.18x spread) -> REJECTED
```

## The bars

| key | cmp | bar | the three runs | spread | margin | why |
|---|---|---|---|---|---|---|
| `bank_low_share` | `ge` | 0.07 | 0.4371 / 0.2584 / 0.2853 | 0.1787 | 0.1884 | unchanged from g2 |
| `bank_height_frac` | `le` | 0.27 | 0.0799 / 0.1711 / 0.1608 | 0.0912 | 0.0989 | unchanged from g2 |
| `o240_area_frac` | `ge` | 0.010 | 0.0130 / 0.0150 / 0.0153 | 0.0023 | 0.0030 | unchanged from g2 |
| `pose_error_max` | `lt` | 0.50 | 0.0000 x3 | 0.0000 | 0.5000 | `GasBankStills.PoseBar` |
| `gas_paint_frac` | `ge` | 0.0130 | 0.0146 / 0.0151 / 0.0153 | 0.0007 | 0.0016 | unchanged; the amount floor, 2.3x the spread |
| `gas_green_share` | `band` | 0.1250-0.2318 | 0.1535 / 0.1426 / 0.1495 | 0.0109 | 0.0176 | NEW: a window. Floor unchanged (smoke fails 3.14x, puff 1.60x); ceiling rejects the zebra (1.17x the spread) and accepts `cards-gas` (1.17x the spread) |
| `bank_paint_share` | `ge` | 0.12 | 0.1855 / 0.1622 / 0.1936 | 0.0314 | 0.0422 | unchanged; zebra 0.0782 fails by 0.0378 |
| `bank_patch_count` | `le` | 25 | 10 / 13 / 7 | 6 | 12 | unchanged; zebra 40 fails by 15 |

## No reshoot

`git diff --stat d1480a099b HEAD -- trench-warfare-3d/Assets` is empty: the build has not moved, so
`run1`/`run2`/`run3` here are copied byte-for-byte from `../ability-fx-captures-r2-2-g2-g1-g2-g2-g2/`, still t1
`sim_tick` 557 / 597 / 637, still twelve distinct hashes, and `one_frame_fault` still refuses two runs on one
frame. No `cards/` here: the isolation stills did not change and stay in
`../ability-fx-captures-r2-2-g2-g1/cards/`. The in-folder look report is therefore skipped (`no cards/`) — the
card REJECTION check reads that folder directly and is not skippable.

## `probe.json`'s `gas_green_share`

`BAR_VALUES['gas_green_share']` is now a tuple, which JSON dumps as a two-element list (`[0.125, 0.2318]`) under
`probe.json`'s `bars` key. Harmless, but read it as one key's floor and ceiling, not two keys.

## The sheet, as a number

`bank_width_m` and `bank_aspect`, unbarred, carried over unchanged:

| run | `bank_width_m` | `bank_rise_m` | `bank_aspect` |
|---|---|---|---|
| run1 (tick 557) | 9.071 | 1.845 | 4.918 |
| run2 (tick 597) | 9.223 | 3.443 | 2.679 |
| run3 (tick 637) | 9.351 | 3.094 | 3.022 |

## Out of scope

Turning the sheet into a puff (a `CombatFx.Banks.cs` change), the owner's `concepts/` brief, and any rescan of
`GREEN_LEAN`.
