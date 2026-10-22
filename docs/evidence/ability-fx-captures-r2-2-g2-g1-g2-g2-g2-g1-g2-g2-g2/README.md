# ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g2-g2-g2: one mass, and a height a player can see, 2026-10-22

**`python probe.py` clears, exit 0.** Three fresh captures of the build at `1c6ab02cae` (`GasBankHeight`, `full`
8, width base 15.5), shot as six batch launches: `TW_GAS_EXTRA_FRAMES` 0, 30 and 60, each with and without
`TW_GAS_PLATE=1`. t1 `sim_tick` 557 / 597 / 637, so no two runs share a frame.

## The owner's complaint

> `bank_patch_count` is still 5, 7 and 4 thick pieces under a ceiling of 10, and `bank_height_frac` is 0.1118,
> 0.2096 and 0.1375, a flat sheet on the mud. Make one patch and give the bank a height a player can see, then put
> both bars outside the spread of three new captures.

## Why the height stopped scaling with the width

A gas card used to be drawn `width * GasBankSquash` tall, and the width is per cell (12-19.3 m), so a narrow cell
drew gas about 1.1 m high and a wide one three times that: the measured spread 0.1118-0.2096 **is** that width
spread, not gas behaviour. `CombatFx.Banks.cs` now has `GasBankHeight(thick)` = 8.4-9.6 m of card whatever the
cell's width; the sheet draws in the card's bottom third, so the gas top stands at 2.8-3.2 m — over a man (1.8 m),
under a roof — and on near the same line in every cell. `GasBankSquash` stays, used only by the isolation still.

One mass came from the same commit: `full` 14 -> 8 (the field peaks near 36, so a cloud's middle reads thick and
the fringes the clouds share do too) and the width base 13.5 -> 15.5, so a card spans four 4 m cells and no fringe
card stands alone.

## The three runs

| key | run1 (t1 557) | run2 (t1 597) | run3 (t1 637) |
|---|---|---|---|
| `bank_patch_count` | 1 | 2 | 1 |
| `bank_height_frac` | 0.3848 | 0.3580 | 0.3490 |
| `bank_rise_m` | 8.609 | 8.035 | 7.819 |
| `bank_paint_share` | 0.9392 | 0.9261 | 0.9277 |
| `bank_low_share` | 0.0578 | 0.0682 | 0.0673 |
| `bank_aspect` | 0.783 | 0.904 | 0.386 |
| `bank_area_frac` | 0.2891 | 0.2732 | 0.2600 |
| `o240_area_frac` | 0.0215 | 0.0241 | 0.0249 |
| `gas_paint_frac` | 0.0183 | 0.0176 | 0.0173 |
| `gas_green_share` | 0.5335 | 0.5255 | 0.5296 |
| `gas_cards` (t1, manifest) | 774 | — | — |

774 cards of `MaxCards` 1536: the card budget risk did not bite, so the standing floor stayed 0.8.

## The bars

Every bar re-set from these three runs by the standing rule — clear the nearest run by at least the runs' own
spread (max - min) — and the old build must fail by more than that spread.

| key | side | before | after | the runs it came from |
|---|---|---|---|---|
| `bank_height_frac_lo` | `ge` | — (new) | **0.313** | min 0.3490 - spread 0.0358 |
| `bank_height_frac` | `le` | 0.31 | **0.421** | max 0.3848 + spread 0.0358 |
| `bank_patch_count` | `le` | 10 | **3** | max 2 + spread 1 |
| `bank_paint_share` | `ge` | 0.175 (half-spread waiver) | **0.913** | min 0.9261 - spread 0.0131 |
| `bank_low_share` | `ge` | 0.15 | **0.047** | min 0.0578 - spread 0.0104 |
| `bank_aspect` | `le` | 4.891 | **1.423** | max 0.904 + spread 0.518 (1.422, +0.001 so binary floating point does not eat the margin) |
| `o240_area_frac` | `ge` | 0.014 | **0.018** | min 0.0215 - spread 0.0034 |
| `gas_paint_frac` | `ge` | 0.0135 | **0.016** | min 0.0173 - spread 0.0010 |
| `gas_green_share` | `ge` | 0.183 | **0.517** | min 0.5255 - spread 0.0080 |
| `pose_error_max` | `lt` | 0.5 | 0.5 | all runs 0.0000 |

`bank_low_share` FELL, and that is the change working: the bank stands, so less of its mask sits under a man's
head. `bank_aspect_lo` is still out of `BARS` — the floor would be negative (min 0.386 - spread 0.518).

**The waiver is gone.** `SPREAD_FACTOR` is now `{}`: the striped sheet needed half-spread on `bank_paint_share`
because the two builds' ranges overlapped. They do not any more — one mass holds 93% of the paint against the old
sheet's 18-19% — so the full-spread rule holds on every key.

## Why the ceiling is 3 and not 1

Two of the three runs make exactly one patch; run2 (frame 30) makes two — its t1 frame still sees a second thick
core. The rule takes the runs as they are: ceiling = max + spread = 2 + 1 = **3**. That is still a hard fail for
the old build's 5 / 7 / 4.

## `--against` the previous build

`python probe.py --against docs/evidence/ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g2-g2` exits 1, as it always
must. Measured with THIS probe:

| run | `bank_patch_count` | `bank_height_frac` | `bank_paint_share` |
|---|---|---|---|
| 1 | 5 | 0.1118 | 0.2037 |
| 2 | 7 | 0.2096 | 0.1886 |
| 3 | 4 | 0.1375 | 0.2152 |

Both of the owner's keys are on the failing side: 7 is over the ceiling 3, and the best height 0.2096 is 0.1034
under the floor 0.313 — 2.9x the old build's own spread. `--selftest` proves exactly this pair of calls without
touching an image.

## The height verdict

r1's critic threw out a bank "not over a man's head". This build's gas top at 2.8-3.2 m replaces that verdict on
the owner's own word in the goal ("give the bank a height a player can see"), so no concepts brief: he named the
change. The `le` roof at 0.421 is the other guard — r1 also threw out `bank_height_frac` 0.32 as a standing wall,
and the measured runs sit between the two.

Out of scope here: the smoke bank, the `GasBank.png` sheet art, and the `cards/` isolation stills (`CARDS_DIR`
still points at `../ability-fx-captures-r2-2-g2-g1/cards`, unchanged, and the card rejection passes against the
new green bar by over 50x the spread).
