# ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g1-g1-g2: stratified depth, wider cards

Three fresh captures of the stratified-jitter build (`CombatFx.Banks.cs` at `6729687f76`), with every bar in
`probe.json` re-set from them. The probe is the previous round's, unchanged except this docstring and the bar
values: `CARDS_DIR` still points at `../ability-fx-captures-r2-2-g2-g1/cards/` and no MEASURE moved, so the
numbers here and there are comparable. This lane does not land.

## The owner's complaint

> The deepest row dip was left unbarred, and `bank_patch_count` went from 21, 21 and 15 to 26, 30 and 19. Bar the
> deepest dip a player sees between the bands, and bring the patch count down so one bank is one patch.

## The two levers

1. **The depth jitter stratified** (`GasBankZOffset`): even card columns sit in the near half of their 4 m field
   cell, odd columns in the far half. Aimed at the dip BETWEEN the rows: every screen row now has a card core
   over its neighbour's soft band edge, instead of all cards wandering freely inside one cell.
2. **Card width base 13.5 -> 18 m, X jitter back to 1.2 m.** Aimed at the patch count: the paint runs on
   sideways and the thin skirt holes that read as their own 200 px islands close.

## The bars

Rule, unchanged: three runs of the SAME build, and a bar must clear the nearest run by at least the three runs'
own spread (`le` bar = max + spread, `ge` bar = min - spread, rounded away from the runs).

| key | run1 / run2 / run3 | spread | bar | margin | previous round's captures |
|---|---|---|---|---|---|
| `bank_low_share` ge | 0.0746 / 0.0777 / 0.0785 | 0.0039 | 0.07 | 0.0046 | 0.0896 / 0.0877 / 0.0912 |
| `bank_height_frac` le | 0.3132 / 0.2814 / 0.2745 | 0.0387 | 0.36 | 0.0468 | 0.3286 / 0.3349 / 0.3214 |
| `o240_area_frac` ge | 0.0182 / 0.0204 / 0.0211 | 0.0029 | 0.015 | 0.0032 | 0.0167 / 0.0190 / 0.0196 |
| `pose_error_max` lt | 0.0000 x3 | 0.0000 | 0.5 | 0.5 | 0.0000 x3 |
| `gas_paint_frac` ge | 0.0213 / 0.0210 / 0.0198 | 0.0015 | 0.018 | 0.0018 | 0.0198 / 0.0188 / 0.0178 |
| `gas_green_share` ge | 0.4634 / 0.4543 / 0.4641 | 0.0098 | 0.44 | 0.0143 | 0.4258 / 0.4229 / 0.4270 |
| `bank_paint_share` ge | 0.7988 / 0.7711 / 0.8165 | 0.0454 | 0.72 | 0.0511 | 0.6161 / 0.6903 / 0.7272 |
| `bank_patch_count` le | 28 / 24 / 18 | 10 | 38 | 10 | 26 / 30 / 19 |
| `row_dark_frac` le | 0.0325 / 0.0461 / 0.0200 | 0.0261 | 0.08 | 0.0339 | 0.0140 / 0.0140 / 0.0000 |
| `row_gap_deepest` | 0.2582 / 0.2686 / 0.2542 | 0.0144 | **none** | - | 0.2980 / 0.3012 / 0.2438 |

Eight bars (`row_gap_deepest` unbarred); the isolation cards are still refused (`cards-smoke` 0.0908 and `cards-puff` 0.1076 against the
`gas_green_share` bar 0.44, by 35.6x and 33.9x the runs' spread).
`python probe.py --against ../ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g1-g1` exits 1 as it must: the previous
round's captures fail `gas_paint_frac` (0.0178), `gas_green_share` (0.4229-0.4270) and `bank_paint_share`
(0.6161-0.7272).

## What the levers did

**The patch count improved**: 28 / 24 / 18 against 26 / 30 / 19, and `bank_paint_share` rose from 0.62-0.73 to
0.77-0.82 — the biggest coherent patch now holds four fifths of the frame's paint, the best this key has read in
any round. Wider cards close the skirt holes, as intended. It is still not the owner's "one bank, one patch":
thin cells at the cloud's edge remain their own 200 px islands, so the bar is set where the captures support it
(38), not at 1.

**`row_dark_frac` moved the WRONG way**: 0.0325 / 0.0461 / 0.0200 against the previous round's
0.0140 / 0.0140 / 0.0000. Stratifying the jitter puts the cards back on a half-cell lattice, which is more
regular than the free jitter it replaced, so the bank's own height holds slightly more dark line than before.
Its bar had to be loosened from 0.03 to 0.08, and at 0.08 the previous round's captures PASS this key. Reported,
not hidden.

**`bank_width_m` grew past the risk threshold**: 35.995 / 32.140 / 31.579 m against 26.18 / 27.76 / 25.93 m. The
plan flagged ~32 m as the point where the paint starts to mislead a player about where the gas field ends. Two of
three runs are over it. The card width base (18 m) is the lever to take back if the owner minds; that is a part-1
change and out of this part's scope.

**`draw_calls`** stays where it was: 131 on gas-t1 with 775 cards (`run1/manifest.json`), against `FlipbookFx.MaxCards` 1536. No lever doubled the cards per cell; the wider cards cost fill rate, not count.

## `row_gap_deepest` still does not clear

A second round running, the key cannot be barred under this folder's own rule. This build reads
0.2582 / 0.2686 / 0.2542, spread 0.0144, so a bar that clears these runs must be 0.2830 or higher, while leaving
the previous round's captures (min 0.2438) failing by more than that spread needs a bar under 0.2294. The two
halves do not overlap. The stratification DID move the key the right way (max 0.3012 -> 0.2686), just not far
enough: the plan's own test was that all three runs read under about 0.20.

**The next lever, in order:** more depth strata — thirds of the cell, offset by `x % 3`, instead of halves. NOT
alpha: `GasBankThin` 0.19 -> 0.28 was tried last round and made both row measures worse, and the reasoning there
(raising the floor alpha raises the bank's peak faster than the seam's) still holds.

## Eye check

`run1/gas-t1.jpg`: one broad green sheet lying on the ground, no horizontal yellow-green banding and no mud
stripe between rows at the game camera's distance — the owner's original complaint reads closed by eye. The one
dark line across the middle of the frame is the flooded ditch in the terrain, not a row seam; it is what
`row_gap_deepest` keeps reading. The sheet is wide: it runs nearly the full width of the frame, which is the
`bank_width_m` number above seen by eye.

## This lane does not land

The puff brief `2026-10-21-gas-bank-turn-the-sheet-into-a-puff` (board id `4101179359`) is still unanswered. Do
not rebase this onto integration or run `land.py` until the owner answers it.

## Out of scope

The gas hue and its colour constants, `DrawSmokeBank`, the stand floor (0.8), `GasBankBandM`,
`GasBankThin`/`GasBankFull`, reshooting `cards/` (`CARDS_DIR` keeps pointing at
`../ability-fx-captures-r2-2-g2-g1/cards/`), turning the sheet into a puff, and landing the lane.
