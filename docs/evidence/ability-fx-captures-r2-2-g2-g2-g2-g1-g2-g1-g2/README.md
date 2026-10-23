# ability-fx-captures-r2-2-g2-g2-g2-g1-g2-g1-g2: the GasPuff band, part 1 of 3, 2026-10-23

**`python probe.py` exits 1.** The nine field bars and three card bars (`cards_gaspuff_rise_m`,
`cards_gaspuff_aspect`, `cards_gaspuff_smoke_rise`) all clear. The fourth, new, `cards_gaspuff_ink_relief`, has no
bar yet - the shader fix (below) moved it from 0.1276 to only ~0.145, still short of the 0.20 floor the owner's own
numbers imply. This folder is part 1 of a 3-part unit; parts 2-3 close the relief gap and bar it.

## The owner's complaint

> The isolation puff is a flat fill, ink relief 0.125 to 0.128, next to a smoke card at 0.32, and the rise bar of
> 8.0 sits 0.007 under the three-capture spread. Give the puff the smoke card's volume and bar that relief outside
> the 0.003 spread. Measure the green puff: the ink mask gives the flat gas card a 7.9 m rise. Start from
> `lane/show/ability-fx-captures-r2-2-g2-g2-g2-g1-g2-g1`'s tip. Bar outside the spread of three captures of the
> current build.

## The fault, found by reading

`Flipbook_URP.shader` line 256: `band = saturate((ink - Low) / (High - Low))`, and `band` lerps the drawing from
shade to light. The `GasPuff` row (`FlipbookFx.cs` line 224) draws **SmokeBank.png** (`Tex = "SmokeBank"`) but
carried **GasBank's** band, `Low 0.17 / High 0.60`; the `SmokeBank` row (line 195) measured that same PNG at
`Low 0.17 / High 0.31`. Twice the span squashed the drawing's ink into the lower half of the band, so the mound
and its rim came out as one value - the flat fill, ink relief 0.1276 against the smoke card's own 0.3238.
`Low`/`High` are facts about the drawing (the `Sheet` comment, lines 95-99), so a row that borrows a PNG must
borrow its band. **Fix: `GasPuff.High` 0.60 -> 0.31**, SmokeBank's own. `Tint`, `Lit 0.75`, `Ink`, `Fill` and
`CombatFx.GasPuffStand` 0.32 are untouched - the green and the field size were already owner-barred.

## What moved, and what didn't

Three fresh `GasBankCards` sets (`cards1`/`cards2`/`cards3`, `TW_CARDS_WAIT_FRAMES` 0/15/30) on the fixed shader:

| card set | cards-gaspuff ink relief | cards-gaspuff green rise | cards-gaspuff green aspect |
|---|---|---|---|
| cards1 | 0.1455 | 7.922 m | 0.7940 |
| cards2 | 0.1433 | 7.906 m | 0.7974 |
| cards3 | 0.1454 | 7.948 m | 0.8026 |

The relief moved from the pre-fix 0.1276 to ~0.145 - real, but still well under the 0.20 floor the owner's own
"next to a smoke card at 0.32" implies. **The band was not the whole fault.** Per this unit's risk note: the next
suspect is the card's own frame, `GasBankCards.PuffFrame` 16, against the smoke card's `CardFrame` 22 - a
different point in the sheet's animation, so a different amount of ink, independent of the band. That is parts
2-3's work, not this one's: this leg changes the shader number and the probe only, as planned.

## Two measures moved mask, on the owner's own words

"Measure the green puff": `cards_gaspuff_rise_m` and `cards_gaspuff_aspect` now read `card_measure`'s **green**
mask instead of the ink one. The ink mask is the plate difference, which on a card hugs the full 12 x 12 box
whatever is drawn inside it - that is why the flat, undrawn `cards-gas` sheet still read an ink rise of 7.874 m
(the box, not the drawing). The green mask is the actual puff pixels, so it is the one that can tell a standing
puff from a flat sheet. `cards_gaspuff_smoke_rise` stays on the **ink** mask: `cards-smoke` is grey, so the green
mask keeps only a few hundred noise pixels on it and cannot compare the two cards at all; the ink ratio therefore
only proves the two cards are drawn in one box, not that the puff stands as tall as the smoke drawing.

`cards_gaspuff_rise_m` and `cards_gaspuff_aspect`'s `BAR_VALUES` are re-set from these three sets' green numbers
(the round number at or below `min - spread`, same rule as every other bar):

| key | cmp | bar | the three (green mask) | spread | margin |
|---|---|---|---|---|---|
| `cards_gaspuff_rise_m` | ge | 7.8 | 7.922 / 7.906 / 7.948 | 0.042 | 0.106 |
| `cards_gaspuff_aspect` | ge | 0.75 | 0.7940 / 0.7974 / 0.8026 | 0.0086 | 0.044 |
| `cards_gaspuff_smoke_rise` | ge | 0.90 (unchanged) | 1.1405 / 1.1739 / 1.0613 (ink) | 0.1126 | 0.1613 |

`cards_gaspuff_ink_relief` has no `BAR_VALUES` entry: `probe.py` prints it per card set and in `CARD_BARS`, but
exits 1 with `cards_gaspuff_ink_relief no bar set yet` until the relief itself clears the owner's 0.20 floor.

## What is here

- `probe.py` - carried from `-g1-g2-g1` with: the shader fix's own measure (`cards_gaspuff_ink_relief`, in
  `card_set`'s `bars` and `CARD_BARS`, unbarred); `cards_gaspuff_rise_m`/`aspect` moved from the ink to the green
  mask; `BAR_VALUES` re-set for those two from this unit's three fresh sets. Everything else - the camera maths,
  `column_heights`, the green mask, `largest_blob`, the O240 clouds, `--pack`, `--calibrate`, `--selftest`, the
  nine field `BAR_VALUES` and `cards_gaspuff_smoke_rise` - is untouched.
- `run1/`, `run2/`, `run3/` - **carried from `-g1-g2-g1` unchanged** (`GasBankStills`, `TW_GAS_WAIT_FRAMES`
  200/215/230). The field draw (`CombatFx.Banks`) is not touched by this unit, so these are still captures of the
  current build and their nine bars still hold.
- `cards1/`, `cards2/`, `cards3/` - **shot fresh in this unit**, on the fixed shader: the five `GasBankCards`
  stills at `TW_CARDS_WAIT_FRAMES` 0/15/30 (`cards-gaspuff` frames 147/162/177 - three frames, not one copied
  three times).
- `probe.json` - the runs' numbers, the frames, the full card report and `BAR_VALUES`.

## The shader change

`trench-warfare-3d/Assets/_Project/Presentation/Camera/FlipbookFx.cs`, the `GasPuff` row: `High = 0.60f` ->
`High = 0.31f`. No other field of that row, and no other row, changed.

## The capture recipe

Unchanged from `-g1-g2-g1`'s README, this checkout's editor closed and the project free
(`python Tools/editor_lock.py status`), one launch at a time; no plate pass (the test shoots its own plate):

```bash
TW_STILLS_DIR=%TEMP%/tw-cards-1 TW_CARDS_WAIT_FRAMES=0 \
python Tools/pipeline/run_detached.py start cards-1 --timeout 2400 -- \
  "C:/Program Files/Unity/Hub/Editor/6000.0.50f1/Editor/Unity.exe" -batchmode \
  -projectPath <checkout>/trench-warfare-3d -runTests -testPlatform EditMode \
  -testFilter GasBankCards -testResults <out>/cards-1.xml -logFile <out>/cards-1.log
python docs/evidence/ability-fx-captures-r2-2-g2-g2-g2-g1-g2-g1-g2/probe.py --pack %TEMP%/tw-cards-1 --into cards1
```

Sets 2 and 3 are the same line at `TW_CARDS_WAIT_FRAMES` 15 and 30, into `cards2` and `cards3`.
