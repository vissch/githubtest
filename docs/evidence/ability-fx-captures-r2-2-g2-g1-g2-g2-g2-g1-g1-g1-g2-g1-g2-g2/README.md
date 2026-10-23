# ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g1-g1-g2-g1-g2-g2: the band scales with the merge step — part 1 of 2

Three fresh captures of the band-scales-with-the-step build (`CombatFx.Banks.cs` at `bfa6ac75f4`, this lane's own
commit), 75 frames apart, with the manifests' `gas_cells` diagnostic now reading the shipped
`CombatFx.GasBankStandFloor` instead of the old `0.8f` literal. `probe.py` is the previous round's file copied
unchanged: `CARDS_DIR` still points at `../ability-fx-captures-r2-2-g2-g1/cards/` and no MEASURE moved, so this
round's numbers compare directly with the last round's. **The bars are still the previous round's**: the re-bar
is part 2.

## The complaint

> At 240 m the bank is a flat striped lime patch on the trench, and the three overview stills are one pose (sim
> ticks 248, 268 and 328). bank_patch_count is 13/19/21 and the ceiling of 29 equals max plus the spread of 8.
> Join the stripes into one cloud a player can read at that zoom, and reshoot the three captures at least 60
> frames apart. Bar bank_patch_count at or under 12.

## The lever (already on the lane, `bfa6ac75f4`)

`GasBankBandFor(step)` scales the painted band with the merge step, so the overview's doubled row rise no longer
outruns the band, and `GasBankStandFloor = GasBankFullThick / 2` (2.0) lifts the thinnest standing card off the
probe's visibility bar.

## The three captures

Three poses, not one: o240 frames **342 / 417 / 492** (75 apart, clearing the owner's 60), t1 sim ticks
**557 / 657 / 757**, o240 sim ticks **249 / 299 / 349**. Every ability call `(taken)`, `pose_error_m` 0.0000
everywhere, `gas_cards` 109 / 120 / 134 at o240 and 352 / 113 / 103 at t1.

| key | run1 / run2 / run3 | spread | previous round |
|---|---|---|---|
| `bank_patch_count` | **8 / 7 / 12** | 5 | 13 / 19 / 21 |
| `gas_green_share` | 0.5743 / 0.5363 / 0.6459 | 0.1096 | 0.5947 / 0.5739 / 0.7019 |
| `row_gap_deepest` | 0.5456 / 0.6114 / 0.2909 | 0.3205 | 0.5025 / 0.6050 / 0.6189 |
| `o240_area_frac` | 0.0311 / 0.0318 / 0.0350 | 0.0039 | 0.0246 / 0.0254 / 0.0301 |
| `bank_low_share` | 0.0582 / 0.0763 / 0.0663 | 0.0181 | 0.0682 / 0.0697 / 0.0804 |
| `bank_height_frac` | 0.4282 / 0.3391 / 0.3479 | 0.0891 | 0.4118 / 0.3930 / 0.3622 |
| `gas_paint_frac` | 0.0266 / 0.0218 / 0.0190 | 0.0076 | 0.0284 / 0.0306 / 0.0241 |
| `bank_paint_share` | 0.9518 / 0.9643 / 0.9175 | 0.0468 | 0.9528 / 0.9351 / 0.9005 |
| `row_dark_frac` | 0.2045 / 0.1728 / 0.0314 | 0.1731 | 0.2245 / 0.1944 / 0.1916 |
| `pose_error_max` | 0.0000 ×3 | 0.0000 | 0.0000 |

## Did the stripes join?

**Yes, by the count the owner named.** `bank_patch_count` fell from 13/19/21 to **8/7/12**: the band now covers
the doubled overview row rise, so the separate painted stripes read as one patch. The worst run is 12, exactly
the owner's ceiling.

**But the ceiling of 12 does not clear this build's own spread.** The spread is 5 (7 to 12), so the probe's
rule (a bar must clear the nearest run by at least the spread) wants `le 17`, which breaks the owner's "at or
under 12". The two instructions cannot both hold on these three captures: the join worked, the run-to-run
scatter did not shrink with it.

`gas_green_share` has the same shape against the owner's 0.44 floor: `min - spread = 0.5363 - 0.1096 = 0.4267`,
which is **under 0.44** for the first time (last round it was 0.4459). The floor is not loosened and not fitted
to the lean run; it is reported here as it came out.

Both are part 2's judgement, named in `note.md` with the next lever. This round's commit changes no bar.

## Checks

- `python probe.py --calibrate` -> exit 0, three runs, three different t1 sim ticks, o240 frames 75 apart.
- `python probe.py` -> exit 1 against the PREVIOUS round's bars, five keys failing the margin rule, both
  isolation cards REJECTED (`cards-smoke` 0.0908, `cards-puff` 0.1076). The re-bar is part 2.

## This lane does not land

The puff brief `4101179359` is still unanswered. This is evidence, not a landing.

Out of scope here: any further change to `CombatFx.Banks.cs`, the row-pitch lever for `row_gap_deepest`,
reshooting `cards/`, the smoke bank, turning the sheet into a puff, and landing the lane.
