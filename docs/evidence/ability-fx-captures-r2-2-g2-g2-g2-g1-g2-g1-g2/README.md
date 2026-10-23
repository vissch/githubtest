# ability-fx-captures-r2-2-g2-g2-g2-g1-g2-g1-g2: the GasPuff card's volume, part 2 of 3, 2026-10-23

**`python probe.py` exits 1.** Every other bar clears; the fourth card bar, `cards_gaspuff_ink_relief`, still has
no bar. Part 1 (the band) took it 0.1276 -> ~0.145; part 2 (`Lit`) takes it to **0.1708 / 0.1671 / 0.1686** - and
that spends both levers the row has, still under the 0.20 floor. **Part 3 is an owner decision, not a code step**
(`docs/reference/decisions.md`, open questions).

## The owner's complaint

> The isolation puff is a flat fill, ink relief 0.125 to 0.128, next to a smoke card at 0.32, and the rise bar of
> 8.0 sits 0.007 under the three-capture spread. Give the puff the smoke card's volume and bar that relief outside
> the 0.003 spread. Measure the green puff: the ink mask gives the flat gas card a 7.9 m rise. Start from
> `lane/show/ability-fx-captures-r2-2-g2-g2-g2-g1-g2-g1`'s tip. Bar outside the spread of three captures of the
> current build.

## Part 1, recap (commit `dac6af7`)

The `GasPuff` row draws `SmokeBank.png` but carried **GasBank's** band, `Low 0.17 / High 0.60`, against the
`SmokeBank` row's own measure of that PNG, `0.17 / 0.31`. `GasPuff.High` 0.60 -> 0.31 moved the relief 0.1276 ->
0.1455 / 0.1433 / 0.1454. Part 1 also moved `cards_gaspuff_rise_m` and `cards_gaspuff_aspect` to the **green**
mask ("measure the green puff") and re-set their bars. It said in as many words: the band was not the whole fault.

## Part 2, step 1: the sheet, measured offline

Does the window the row actually plays sit above its band? `Low` 0.17 / `High` 0.31, the shader's
`band = saturate((ink - Low) / (High - Low))`, and `ink = luma(rgb) / max(a, 0.06)` (shader line 154). Per cel of
`SmokeBank.png`, pixels with alpha > 128, each cel resized to 96 x 96:

| cel | px | ink p10 | p50 | p90 | share < 0.17 | share > 0.31 |
|---|---|---|---|---|---|---|
| 12 | 518 | 0.180 | 0.231 | 0.390 | 0.042 | 0.158 |
| 13 | 484 | 0.185 | 0.237 | 0.454 | 0.029 | 0.219 |
| 14 | 473 | 0.179 | 0.239 | 0.467 | 0.040 | 0.230 |
| 15 | 471 | 0.184 | 0.240 | 0.507 | 0.030 | 0.251 |
| **16** (`PuffFrame`) | 402 | **0.178** | **0.238** | **0.541** | 0.042 | 0.254 |
| 17 | 386 | 0.180 | 0.238 | 0.529 | 0.036 | 0.262 |
| 18 | 317 | 0.188 | 0.239 | 0.510 | 0.006 | 0.256 |
| 19 | 277 | 0.185 | 0.237 | 0.467 | 0.014 | 0.202 |
| 20 | 201 | 0.191 | 0.239 | 0.435 | 0.010 | 0.224 |
| 21 (`CardFrame` 22 clamps here) | 208 | 0.190 | 0.241 | 0.431 | 0.014 | 0.226 |
| 12-21 pooled | | 0.183 | 0.238 | 0.490 | 0.030 | 0.227 |

**The answer is no.** The played window (`CombatFx.Banks.SmokeBankFirst` 12 .. `SmokeBankLast` 20.5, and the card's
own `GasBankCards.PuffFrame` 16) spans 0.17-0.31 the way the `SmokeBank` row measured it: only 3% of the ink falls
under `Low`, 23% over `High`, and the p50 lands mid-band (band 0.49). Nothing is clipped, so **the band is
innocent** - and the card's frame 16 has a *wider* ink spread than the smoke card's frame 21. No band change was
due, and none was made.

## Part 2, step 2: the one change - `Lit` 0.75 -> 1

With the band right, the only difference left from `cards-smoke` is `Lit`. `color = Tint * lerp(ink, lit, Lit)`
(shader line 258): at 0.75 a quarter of the **raw, unshaded** ink is mixed back over the shading, washing it out.
The `SmokeBank` row sets no `Lit`, which means 1 (`FlipbookFx.cs` line 661:
`Additive || Fire ? 0 : Lit > 0 ? Lit : 1`). `GasPuff.Lit` is now **1**, SmokeBank's own. Nothing else of that row,
and no other row, changed.

**It cost no green.** The risk note said to stop if the moon turned the puff grey. Over the card's changed pixels:

| | mean R | mean G | mean B | mean G - max(R, B) |
|---|---|---|---|---|
| `Lit` 0.75 (part 1's cards1) | 67.9 | 77.0 | 83.8 | -17.3 |
| `Lit` 1 (part 2's cards1) | 71.3 | 81.9 | 87.1 | **-15.8** |

Slightly greener and slightly brighter, not greyer. The green mask keeps the same pixels (29918 -> 30095 px).

## Part 2, step 3: three fresh card sets

`cards1`/`cards2`/`cards3`, `TW_CARDS_WAIT_FRAMES` 0/15/30, `cards-gaspuff` frames **147 / 162 / 177** (three
frames, not one copied three times). `run1`-`run3` are still carried unchanged - no field draw moved.

| card set | gaspuff ink relief | gaspuff green relief | gaspuff green rise | green aspect |
|---|---|---|---|---|
| cards1 | **0.1708** | 0.1487 | 7.948 m | 0.7949 |
| cards2 | **0.1671** | 0.1484 | 7.948 m | 0.7906 |
| cards3 | **0.1686** | 0.1483 | 7.948 m | 0.7949 |

Spread 0.0037. Every other bar clears with its old value (`cards_gaspuff_rise_m` ge 7.8, margin 0.148; `aspect`
ge 0.75, margin 0.0406; `smoke_rise` ge 0.90, margin 0.1613; the nine field bars unmoved).
`cards_gaspuff_ink_relief` is still printed and still unbarred, so `probe.py` exits 1.

## Why 0.20 is not reachable from this row, and why it may be the wrong target

Two findings, both measured:

1. **No band reaches it either.** Fitting the capture's luma to the band (`luma = 38.2 + 80.5 * band`, from five
   percentiles of cards1 against cel 16's ink) and re-predicting the relief for other bands: `0.17/0.31` 0.374,
   `0.18/0.49` 0.420, `0.19/0.28` 0.415, `0.20/0.26` 0.432, `0.23/0.24` 0.503 - on the whole changed area, where
   the measured number is 0.441. Even an absurdly narrow band (which would be a lie about the drawing, and would
   posterise the mound into two flat tones) buys about +15%: the probe's ink relief ~0.17 -> ~0.20 at the very best.
2. **The 0.32 the floor comes from is mostly background, not shading.** The ink mask is the plate difference inside
   one blob. `cards-gas` - the card everyone agrees draws nothing - reads the *highest* ink relief of all four,
   0.3290, above `cards-smoke` 0.3238 and `cards-puff` 0.3182. Those three cards are faint and translucent, so
   their mask is mostly dark night terrain showing through and its variation is the terrain's. The gaspuff card is
   an opaque mound that covers its box, so its mask is the drawing's own luma. Measured on the whole changed area
   instead (no blob), the ordering reverses: gaspuff CV **0.4408** against smoke **0.3685**. The puff already has
   more relief than the smoke card; the 0.32 is not a like-for-like target.

So the honest reading: the puff now has the smoke card's band and the smoke card's `Lit` - the smoke card's
volume, as asked - and the remaining gap to 0.20 is in the measure, not in the drawing. Changing the measure to
suit the number, or narrowing the band off its own measurement, are both owner calls, and they are in
`docs/reference/decisions.md` under the open questions.

## The code change

`trench-warfare-3d/Assets/_Project/Presentation/Camera/FlipbookFx.cs`, the `GasPuff` row: `Lit = 0.75f` ->
`Lit = 1f`, with the cel percentiles in the row's comment. `Tex`, `Tint`, `Low`, `High`, `Ink`, `Fill` and
`CombatFx.GasPuffStand` 0.32 are untouched.

## What is here

- `probe.py` - part 1's, plus part 2's finding in the docstring and at the `BAR_VALUES` foot, and the `aspect` /
  `smoke_rise` comments re-quoted from part 2's three sets. **No measure, mask or bar value changed**, so
  `--selftest` still exits 0 and the nine field bars are untouched.
- `run1/`, `run2/`, `run3/` - carried from `-g1-g2-g1` unchanged (`GasBankStills`, `TW_GAS_WAIT_FRAMES`
  200/215/230). The field draw is not touched by this unit.
- `cards1/`, `cards2/`, `cards3/` - **shot fresh in part 2**, on `Lit = 1`.
- `probe.json` - the runs' numbers, the frames, the full card report and `BAR_VALUES`.

## The capture recipe

Unchanged, this checkout's editor closed and the project free (`python Tools/editor_lock.py status`, from
`trench-warfare-3d/`), one launch at a time; no plate pass (the test shoots its own plate):

```bash
TW_STILLS_DIR=%TEMP%/tw-cards-1 TW_CARDS_WAIT_FRAMES=0 \
python Tools/pipeline/run_detached.py start cards-1 --timeout 2400 -- \
  "C:/Program Files/Unity/Hub/Editor/6000.0.50f1/Editor/Unity.exe" -batchmode \
  -projectPath <checkout>/trench-warfare-3d -runTests -testPlatform EditMode \
  -testFilter GasBankCards -testResults <out>/cards-1.xml -logFile <out>/cards-1.log
python docs/evidence/ability-fx-captures-r2-2-g2-g2-g2-g1-g2-g1-g2/probe.py --pack %TEMP%/tw-cards-1 --into cards1
```

Sets 2 and 3 are the same line at `TW_CARDS_WAIT_FRAMES` 15 and 30, into `cards2` and `cards3`.
`run_detached.py` has no `wait`: poll `status <name>` until it prints `DONE`.

The cel table above is reproduced with
`python docs/evidence/ability-fx-captures-r2/sheetink.py trench-warfare-3d/Assets/_Project/Resources/VFX/SmokeBank.png 8 4 22`
for the means, and the percentiles with a read-only PIL crop per cel (the formula is in step 1 above).
