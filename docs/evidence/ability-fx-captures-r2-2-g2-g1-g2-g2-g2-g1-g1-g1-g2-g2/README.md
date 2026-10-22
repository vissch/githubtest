# ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g1-g1-g2-g2: the ditch is not the dark row (both levers rebutted)

**This round sets no bars and the probe here does not pass: `probe.py` is PREV's, unchanged, and there are no
`run*/` folders for it to read.** The round's two levers — mask the ditch, or move the bank off it — were both
tried and both are rebutted by measurement, which is the result reported here. PREV is
`../ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g1-g1-g2` (the shipped build: `CombatFx.Banks.cs` with the
thirds-of-the-cell jitter and the 14.5 m width base, commit `59fcb4c9b4`). This lane does not land.

## The owner's complaint

> The deepest row dip is unbarred (0.2582, 0.2686, 0.2542) and the dark line is the flooded ditch, so the probe
> scores terrain. Mask that ditch or move the bank off it, stagger the cards in thirds of the cell, and bar the
> seam a player sees. Pull the card width back so the bank stays under 32 m.

The thirds and the width pullback were part 1 and are in the build (`59fcb4c9b4`). This part is about the ditch.

## Lever 1, the terrain mask: the plate has no ditch where the dip is

A terrain mask was written into `row_profile_stats` exactly as planned (a PLATE row profile over the same
`x_lo..x_hi` span; a row is TERRAIN when its plate mean departs from the plate's own local median over 41 rows by
6.0 luma or more, grown 3 rows; terrain rows dropped before the median, the trough count and the deepest dip;
`row_terrain_frac` reported and a 0.25 ceiling that FAILS the probe). It was then calibrated on PREV's three runs
before any capture, as the plan demands. It masked **nothing**: `row_terrain_frac` 0.0000 on all three runs, and
the deepest dip rows (286 / 380 / 376) were not in a masked band.

The plate profile says why. Over the bank's own column span, on PREV's three runs:

| run | deepest dip row | the PLATE's departure from its own local median at that row | the plate's biggest departure anywhere |
|---|---|---|---|
| run1 | 286 | +0.28 luma | 4.71 (row 487) |
| run2 | 380 | +0.62 luma | 3.89 (row 719) |
| run3 | 376 | +1.34 luma | 3.67 (row 468) |

The dip rows are among the FLATTEST rows of the plate. The flooded ditch is in the frame and is visible by eye,
but it is not at the row the probe scores, and nothing in the plate at that row is dark: **`row_gap_deepest` is
reading a card seam, not terrain.** Per the plan ("do not tune `ROW_TERRAIN_BAR` until the dip is inside the mask
— that is fitting the bar to the answer") the mask was deleted and `row_profile_stats` left as PREV has it.

## Lever 2, moving the bank: it makes every key worse, and the site is unstable

The plan's fallback (step 11) was built: `GasBankStills.Open` grew a flatness rule, the ground height spread
within `r` under `FlatBarM` = 1 m, so the gas centre must stand on flat ground. The centre moved from **(56, 142)
to (20, 102)** — the only open AND flat patch is at the left edge of the field, not near the middle — and three
captures were shot there (t1 `sim_tick` 557 / 597 / 637, extras 0 / 30 / 60, the same 40-tick spacing as PREV).
They are kept here as `moved-run1/`, `moved-run2/`, `moved-run3/` with their numbers in `moved-probe.json`. They
are deliberately NOT named `run*`: they are not captures of the shipped build, since the flatness rule was
reverted after they were read.

| key | moved-run1 / 2 / 3 | spread | PREV's three runs | PREV's spread |
|---|---|---|---|---|
| `row_gap_deepest` | 0.4411 / 0.4665 / 0.4285 | 0.0380 | 0.2582 / 0.2686 / 0.2542 | 0.0144 |
| `row_dark_frac` | 0.1550 / 0.1522 / 0.0825 | 0.0725 | 0.0325 / 0.0461 / 0.0200 | 0.0261 |
| `gas_green_share` | 0.3201 / 0.6548 / 0.7457 | 0.4256 | 0.4634 / 0.4543 / 0.4641 | 0.0098 |
| `gas_paint_frac` | 0.0400 / 0.0152 / 0.0361 | 0.0248 | 0.0213 / 0.0210 / 0.0198 | 0.0015 |
| `bank_paint_share` | 0.8219 / 0.8252 / 0.8207 | 0.0045 | 0.7988 / 0.7711 / 0.8165 | 0.0454 |
| `bank_patch_count` | 39 / 27 / 32 | 12 | 28 / 24 / 18 | 10 |
| `bank_width_m` | 26.136 / 32.767 / 30.552 | 6.631 | 35.995 / 32.140 / 31.579 | 4.416 |
| `bank_low_share` | 0.1269 / 0.1050 / 0.1085 | 0.0219 | 0.0746 / 0.0777 / 0.0785 | 0.0039 |

Two facts kill the move:

1. **The seam got worse, not better.** `row_gap_deepest` went from 0.25-0.27 to 0.43-0.47 and `row_dark_frac` from
   0.02-0.05 to 0.08-0.16. If the dip were the ditch, moving off the ditch would have removed it; it doubled. That
   is the second, independent rebuttal of the owner's premise.
2. **The new site cannot carry a bar at all.** `gas_green_share` reads 0.3201 / 0.6548 / 0.7457 — a spread of
   0.4256 against PREV's 0.0098 — and `gas_paint_frac` swings 2.6×. At the field's left edge the cloud drifts over
   ground that differs frame to frame, so three captures of ONE build disagree more than two different builds do.
   Under this folder's rule (a bar must clear the nearest run by at least the runs' spread) every bar re-set from
   these three runs would be meaningless or negative.

So the flatness rule was reverted (`GasBankStills.cs` is back at `59fcb4c9b4`) and no bar in this folder was
re-set. `bank_width_m` is the one key the move flattered (26.1-32.8 m against PREV's 31.6-36.0 m), but it comes
with that 6.6 m spread, so it is no answer to the owner's 32 m either; part 1's width pullback (18 -> 14.5 m base)
is the lever for that and still wants three captures at the OLD centre to be measured.

## What the next round should do

The owner's named cause is rebutted twice over, so the next lever is not terrain:

- **Measure PREV's shipped build (thirds + 14.5 m base) at the old centre (56, 142)**: three captures, every bar
  re-set, `bank_width_m` barred. Part 1 landed the thirds and the pullback but nothing has photographed them yet
  — that is the cheapest open work in this lane, and it is what tells whether the thirds moved
  `row_gap_deepest` at all.
- **Then, for the seam**: the dip is a card seam in Z, so the remaining levers are in the card, not in the field —
  a second card per cell at half pitch, or a taller band. `GasBankThin` is NOT a lever (tried and reverted two
  rounds ago, it made both row measures worse).
- **Do not retry the mask or the move** without new evidence: both are measured above.

A decision for the owner is in `docs/reference/decisions.md` ("Open: waiting on the owner"): his stated cause for
the dark row is not in the data, and whether to keep chasing it is his call.

## This lane does not land

The puff brief `2026-10-21-gas-bank-turn-the-sheet-into-a-puff` (board id `4101179359`) is still unanswered. Do
not rebase this onto integration or run `land.py` until the owner answers it.

## Out of scope

The gas hue and its colour constants, `DrawSmokeBank`, `GasBankBandM`, `GasBankThin`/`GasBankFull`, reshooting
`cards/` (`CARDS_DIR` still points at `../ability-fx-captures-r2-2-g2-g1/cards/`), turning the sheet into a puff,
and landing the lane.
