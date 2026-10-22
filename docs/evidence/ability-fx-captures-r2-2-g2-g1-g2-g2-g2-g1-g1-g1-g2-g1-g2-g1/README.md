# ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g1-g1-g2-g1-g2-g1: a level-topped band, and the two bars held RED

**`python probe.py` exits 1.** Eight of ten bars are green. The owner's two row keys are held at the numbers he
gave, 0.40 and 0.08, and this build misses both: `row_gap_deepest` 0.3585 / 0.4607 / 0.3776 and `row_dark_frac`
0.1267 / 0.1705 / 0.1653. Neither was loosened to fit these three captures, and the measure was not retuned.
Both moved a long way the RIGHT way; what is left is a different thing from the cut the owner saw.

## The complaint (the owner's words)

> At the game camera the gas is still a striped sheet with a dark cut through the lower middle. row_gap_deepest
> reads 0.5025/0.6050/0.6189, about double the previous round, and the bar 0.74 sits on max plus the spread.
> row_dark_frac reads 0.2245/0.1944/0.1916 and the bar was loosened from 0.09 to 0.26. Close the cut and the
> stripes so the sheet reads as one mass. Bar row_gap_deepest at or under 0.40 and row_dark_frac at or under
> 0.08. Do not loosen either bar to fit these three runs.

## The lever

A card's band is now **level-topped, not ground-following** (`GasBankBandFor`, commit `87bcd5a803`). A card
standing in a dip is drawn taller by how far its own ground lies under the highest ground one 4 m cell around it,
clamped at `GasBankFillMaxM` 2.5 m, so its top sits level with its neighbours' instead of sinking with the ground.
The old fixed 1.78 m band standing on each card's own ground was the dark cut: where the ground steps up going
away from the camera (a trench lip, a crater rim) the row behind stood higher on screen than the card in front
reached, and bare mud showed between them. Nine ground samples per standing cell once a sim tick; no new card and
no new draw call. The smoke bank is untouched (`fill: false`), and the isolation cards draw as before, so `cards/`
was not reshot.

## Did the cut close? Yes. Is it enough? No.

`row_min_y_frac` is new and unbarred: where in the core the deepest row sits, 0 the far edge (away from the
camera), 1 the near edge. It reads **0.0703 / 0.0228 / 0.0000**. The previous build's deepest rows sat at screen
y 225 / 225 / 453 of 720 — holes INSIDE the sheet, at the same y under the same camera every run. Those holes are
gone. The deepest row is now the bank's own **far shoulder**, the first row of the core, where the gas simply
ends. `row_gap_deepest` and `row_dark_frac` cannot tell a shoulder from a hole, so they still count it.

## The bars

Set from these three runs alone (t1 `sim_tick` 556 / 637 / 717, three distinct frames) by this folder's rule: an
`le` bar at or above max + spread, a `ge` bar at or below min - spread, rounded away from the runs. The two row
keys are the exception and are the owner's numbers, not the rule's.

| key | run1 / run2 / run3 | spread | bar | margin | previous round's captures |
|---|---|---|---|---|---|
| `bank_low_share` | 0.0583 / 0.0599 / 0.0629 | 0.0046 | ge 0.053 | 0.0053 | 0.0682 / 0.0697 / 0.0804 |
| `bank_height_frac` | 0.4573 / 0.4396 / 0.4339 | 0.0234 | le 0.49 | 0.0327 | 0.4118 / 0.3930 / 0.3622 |
| `o240_area_frac` | 0.0296 / 0.0326 / 0.0355 | 0.0059 | ge 0.023 | 0.0066 | 0.0246 / 0.0254 / 0.0301 |
| `gas_paint_frac` | 0.0225 / 0.0207 / 0.0214 | 0.0018 | ge 0.018 | 0.0027 | 0.0284 / 0.0306 / 0.0241 |
| `gas_green_share` | 0.6437 / 0.6350 / 0.7348 | 0.0998 | ge 0.44 (owner's floor) | 0.1950 | 0.5947 / 0.5739 / 0.7019 |
| `bank_paint_share` | 0.9802 / 0.9782 / 0.9560 | 0.0242 | ge 0.93 | 0.0260 | 0.9528 / 0.9351 / 0.9005 |
| `bank_patch_count` | 6 / 7 / 10 | 4 | le 14 | 4 | 13 / 19 / 21 |
| `row_dark_frac` | 0.1267 / 0.1705 / 0.1653 | 0.0438 | **le 0.08 (owner's, RED)** | -0.0905 | 0.2245 / 0.1944 / 0.1916 |
| `row_gap_deepest` | 0.3585 / 0.4607 / 0.3776 | 0.1022 | **le 0.40 (owner's, RED)** | -0.0607 | 0.5025 / 0.6050 / 0.6189 |
| `pose_error_max` | 0.0000 ×3 | 0.0000 | lt 0.5 | 0.5 | 0.0000 |
| `row_min_y_frac` (unbarred) | 0.0703 / 0.0228 / 0.0000 | 0.0703 | — | — | not measured |

The honest ceilings by the rule would have been `row_gap_deepest` **le 0.57** and `row_dark_frac` **le 0.22**.
They are not written: the goal forbids loosening, and `ROW_CORE_TRIM`, `ROW_TROUGH_FLOOR` and `ROW_EXTENT_FLOOR`
were not touched either, because retuning the measure is the same loosening in disguise. Two of the three runs
are already under 0.40 on `row_gap_deepest`; run2 (0.4607) is not, and the spread is 0.1022, so the build is not
close in the honest sense even though the picture is.

## What moved the wrong way

- `bank_height_frac` 0.4339-0.4573 against 0.3622-0.4118: the sheet is taller on screen, as a 2.5 m fill must
  make it. Under the bar, and `bank_low_share` says the bank is still on the ground.
- `gas_paint_frac` fell to 0.0207-0.0225 from 0.0241-0.0306: the filled cards overlap more, so the same gas
  covers slightly fewer DIFFERENT pixels. `bank_paint_share` rose to 0.956-0.980 and `bank_patch_count` fell to
  6-10 from 13-21 — the fringe islands merged, which is the one mass the owner asked for.

## The eye check (`run1/gas-t1.jpg`)

It reads as one pooled mass. No dark cut through the lower middle, no stripe pattern, the top edge level across
the trench lips, wrecks and stakes standing out of it. The risk that a card in a 2.5 m dip is drawn ~12.8 m tall
and stretches the book's art does **not** show at this zoom: the texture is soft enough that the stretch is
invisible. `GasBankFillMaxM` does not need lowering.

## The next lever (not taken here, on purpose)

`GasBankLoad`'s `max(own, mean)` — lift the fringe instead of trimming it, so the far shoulder steepens and the
shoulder stops setting both row keys. Both earlier READMEs name it. It was left out of this round because it
makes a lone cell a plate, which breaks `TheGasBankEdgeDrawsFromItsNeighbourhood` and would move
`bank_patch_count`. A second candidate, cheaper: give `row_profile_stats` a shoulder-aware core so a monotone
far edge is not counted as a trough — but that is a MEASURE change and must be agreed with the owner first,
since it reads as loosening.

## Checks

- `python probe.py` -> exit 1 (`row_dark_frac` and `row_gap_deepest` RED, the other eight OK, both isolation
  cards REJECTED at 0.0908 and 0.1076 against 0.44).
- `python probe.py --against ../ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g1-g1-g2-g1-g2` -> exit 1, printing
  the previous build's 0.5025 / 0.6050 / 0.6189 and 0.2245 / 0.1944 / 0.1916.
- `python probe.py --selftest` -> exit 0.
- `gate.ps1 -EditOnly` -> exit 0.

## This lane does not land

Puff brief `4101179359` is unanswered.
