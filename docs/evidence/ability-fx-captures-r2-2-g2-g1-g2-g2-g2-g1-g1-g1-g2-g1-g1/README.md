# ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g1-g1-g2-g1-g1: max(own, mean) lifts the fringe

Three fresh captures of the max(own, mean) build (`CombatFx.Banks.cs`), with every bar in `probe.py` re-set from
these three runs alone. `probe.py` is the previous round's file with only its FRAME paragraph and `BAR_VALUES`
changed: `CARDS_DIR` still points at `../ability-fx-captures-r2-2-g2-g1/cards/` and no MEASURE moved, so this
round's numbers and the last round's compare directly.

## The complaint

> The neighbourhood mean trims the fringe and leaves a new torn edge. At the game camera, gas-t1 run1, run2 and
> run3 are a wide green sheet with a ripped bottom fringe and dark gaps, not one patch. `bank_patch_count` is
> 29/18/21 and the ceiling of 40 is looser than last round's 38, so the probe exits 0 on a shredded sheet. Change
> `GasBankLoad` to `max(own, mean)` so the outer ring joins the mass. Bar `bank_patch_count` under 38.

## The lever

`GasBankLoad` no longer returns the plain mean. It returns `Mathf.Max(self, (self + ringSum) / GasBankNeighbourhood)`:
a fringe cell is LIFTED to its neighbourhood's thickness instead of being trimmed down to it, so the outermost ring
should draw thick enough to join the body it sits against instead of fading out below the paint threshold between
its own neighbours. Inside a solid body the core is unchanged (max of equals, since nine times the same value over
nine is that value), so this agrees with the old mean-only formula everywhere the mean did not need to trim.

## It did not work either

`bank_patch_count` is **35 / 30 / 24** against the previous (mean-only) round's 29 / 18 / 21. The worst count is
**35** and the spread is **11**, so this folder's own rule gives a ceiling of **35 + 11 = 46** — looser again, not
tighter, than both the mean-only round's 40 and the original plain-cell round's 38. The bar is set where the
captures support it and not a pixel lower; no bar was loosened or tightened to make the count look better.

**Why the lift did not close it either.** This is the risk the plan itself named before shooting: `max` pushes the
near-threshold ring one cell outward rather than merging it with the body. A fringe cell whose own reading is below
its neighbourhood's mean is lifted to exactly that mean (since `self < mean` means `max(self, mean) == mean`) — the
same value the plain mean would have given it. The lift only changes anything for a cell whose own reading is
*already above* its neighbourhood's mean (where the old mean would have trimmed it and `max` now does not), and
that is not where the ragged outermost ring lives. The islands are still a property of the outermost ring itself,
not of which formula computes its thickness, so a blend of this family cannot fully close them.

**The next lever.** Not a second blend: the plan's own risk note names the stand floor as what to try next
(`DrawGasBank`'s `Refresh` cell floor of `0.8`), out of scope for this leg.

## The bars

Every value from `probe.json`, t1 sim_tick 752 / 866 / 947. Rule unchanged: a bar must clear the nearest run by
at least the three runs' own spread (`le` bar = max + spread, `ge` bar = min − spread, rounded away from the runs).

| key | run1 / run2 / run3 | spread | bar | margin | previous round's captures |
|---|---|---|---|---|---|
| `bank_patch_count` | 35 / 30 / 24 | 11 | le 46 | 11 | 29 / 18 / 21 (bar was 40) |
| `bank_paint_share` | 0.7621 / 0.8006 / 0.8458 | 0.0837 | ge 0.67 | 0.0921 | 0.7625 / 0.8295 / 0.7168 |
| `gas_paint_frac` | 0.0324 / 0.0424 / 0.0343 | 0.0100 | ge 0.022 | 0.0104 | 0.0216 / 0.0200 / 0.0212 |
| `gas_green_share` | 0.4616 / 0.3995 / 0.3725 | 0.0891 | ge 0.28 | 0.0925 | 0.4726 / 0.4688 / 0.6139 |
| `bank_low_share` | 0.0840 / 0.0791 / 0.0612 | 0.0228 | ge 0.038 | 0.0232 | 0.0829 / 0.0723 / 0.0873 |
| `bank_height_frac` | 0.3433 / 0.2725 / 0.3112 | 0.0708 | le 0.42 | 0.0767 | 0.2725 / 0.3055 / 0.2939 |
| `o240_area_frac` | 0.0213 / 0.0295 / 0.0347 | 0.0134 | ge 0.007 | 0.0143 | 0.0209 / 0.0234 / 0.0254 |
| `row_dark_frac` | 0.3077 / 0.2677 / 0.1463 | 0.1614 | le 0.47 | 0.1623 | 0.0194 / 0.0066 / 0.0461 |
| `pose_error_max` | 0.0000 ×3 | 0.0000 | lt 0.5 | 0.5 | 0.0000 |

`row_gap_deepest` stays **unbarred**. The runs are 0.6396 / 0.6358 / 0.4660, spread 0.1736, so a bar that clears
them must sit at 0.8132 or above, while the previous round's captures (max 0.2761) sit far under that value and
would PASS it rather than fail. The two halves of the bar rule still do not overlap, so this round reports the key
and leaves it unbarred rather than fake a bar.

## What moved the wrong way

- **`bank_patch_count`** loosened again: 35/30/24 against 29/18/21, ceiling 46 against 40. The lift did not merge
  the outer ring with the body (see "It did not work either" above).
- **`row_dark_frac`** got much worse: 0.1463-0.3077 against the previous round's 0.0066-0.0461, a roughly 7x
  jump. The lifted fringe draws a wider, thicker outer ring, and that ring's own internal banding (still one card
  per 4 m row in depth) now spans more of the bank's own on-screen height, so more of that height reads as a dark
  mud line between rows.
- **`row_gap_deepest`** moved the same way, worse: 0.4660-0.6396 against 0.2502-0.2761, and still cannot be
  barred for the same structural reason as the last two rounds.
- **`bank_width_m`** (unbarred, not printed as a bar key but visible in `probe.json`): the bank is wider again,
  consistent with the plan's own prediction that `max` would pull the width back out by about one ring versus the
  mean build.
- **`gas_green_share`** loosened further: 0.28 against the previous round's 0.32. Both isolation cards still
  REJECTED, with more margin than before (2.12x and 1.93x the spread, against 1.58x/1.46x last round), because the
  new bar sits lower while the cards' own numbers are unchanged.
- **`bank_low_share`**, **`bank_height_frac`** and **`o240_area_frac`** all moved in the same direction as last
  round (slightly looser bars), tracking the wider, thicker bank rather than any new effect.

## The eye check

`run1/gas-t1.jpg` at the game camera: the gas still reads as a wide green sheet, visibly thicker and taller at the
fringe than the mean-only build's capture, but the bottom edge is still broken into distinct lighter blotches
rather than one smooth plate — consistent with `bank_patch_count` not improving. The lift makes the ring brighter,
not more connected to its neighbours.

## Checks

- `python probe.py` → exit 0, every bar OK, both isolation cards REJECTED.
- `python probe.py --against ../ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g1-g1-g2-g1` → exit 1, as it must.

## This lane does not land

The puff brief `4101179359` is unanswered: the owner has not said whether the gas should stay a ground-hugging
sheet at all or become a puff. Until he answers, this is evidence, not a landing.

Out of scope here: the smoke bank, `GasBankThin`/`GasBankFull`, the stand floor, reshooting `cards/`, `tasks.md`'s
probe citation, turning the sheet into a puff, and landing the lane.
