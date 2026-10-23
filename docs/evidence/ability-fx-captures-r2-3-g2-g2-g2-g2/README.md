# The chlorine bank, round 3-g2-g2-g2-g2: the moon stops lighting the gas, and the owner's number moves

The owner (2026-10-23): "Make the pixels one hue. The blob hue I named is still 126.5, 106.2 and 148.8. The pass
reports an unmixed 80.0, 80.9 and 80.0, and the bar of 18.5 is that max plus 5."

Both readings were the cause, not a complaint about the measure. The blob hue he named sat 39-81 deg off the
paint's own 67.5, and the alpha-free `o240_unmix_hue_deg` - the shot-minus-plate direction, which no card alpha
can touch - sat at 80 deg on all three seeds. A number that is wrong even with the blend taken out is the
drawing's colour, so the lever had to be the light on the card, not the card's coverage.

## The lever: `NightGas`

`FlipbookFx.NightGas(Color)` (new, beside `NightSmoke` / `NightEarth`) paints `Book.GasBank` the way
`PaintNight` paints the night smoke: no ink in the light band, a plain grey shade, `_ShadeMood = 0`,
`_Lit = NightLit`. `CombatFx.ApplyTints` calls it with `GasBankNight` on moonlit fields only, after the biome
tint. The drawn colour is then a scalar on the paint, so a blob pixel keeps the paint's hue instead of picking
up the moon's blue key (0.56, 0.70, 1.0).

`PaintNight` itself is shared with Burst, Smoke and Column and was not touched; this is a new caller.

## What it did to the owner's number

| `o240_blob_hue_deg` | s0 | s1 | s2 | spread |
|---|---|---|---|---|
| before (`235f4414ab`, `_Solid = 0.6`, moon on the gas) | 123.3 | 104.9 | 143.3 | 38.4 |
| the owner's own reading of the round before | 126.5 | 106.2 | 148.8 | 42.6 |
| **after (`NightGas`, the stills here)** | **85.0** | **76.2** | **99.7** | **23.5** |

The core reading moves further: `o240_bank_hue_deg`, the hue of the blob's most opaque quartile, goes from
88.7 / 85.7 / 98.2 to **69.5 / 68.7 / 71.1** - within 4 deg of the paint's 67.5. The alpha-free
`o240_unmix_hue_deg` goes from 79.5 / 80.0 / 79.4 to **61.4 / 61.4 / 58.1**. What is left in the whole-blob mean
is the blob's faint outer rim, whose raw colour is still mostly the blue-dominant grass behind it
(`o240_grass_hue_deg` 221-223).

## `_Solid` was the wrong lever, and the alpha was never the problem

The round before pulled the card's alpha near-binary (`_Solid = 0.6`) and the owner's number moved three to
five degrees. It could not have moved more: the median alpha over the blob was already **0.72-0.94**, so there
was little blend left to remove. Simulating a fully binary alpha on those same stills gives 69.7 / 71.1 / 72.1
against the hue rotation's 70.4 / 73.3 / 73.0 - no gain. `_Solid` stays at 0.6 (it is a real improvement to the
silhouette and its test stands), but it is not what this reading needed.

## This run, against the bars

No bar and no measure changed this round. The two bars on the owner's reading were set last round from three
captures of the OLD build, deliberately outside their spread, and are left exactly where they were.

| Bar | s0 | s1 | s2 | Want | |
|---|---|---|---|---|---|
| `t1_changed_frac` | 0.3703 | 0.3501 | 0.3845 | >= 0.08 | OK |
| `texture_var_drop` | 0.6135 | 0.7125 | 0.6955 | >= 0.40 | OK |
| `o240_hue_dist_deg` | 153.12 | 154.65 | 149.66 | >= 103.5 | OK |
| `side_face_px` | 231335 | 93197 | 243203 | >= 41000 | OK |
| `o240_hue_off_deg` | 6.07 | 6.10 | 9.44 | <= 18.5 | OK |
| `o240_blob_off_deg` | 17.53 | 8.74 | 32.16 | <= 33.5 | OK |
| `o240_blob_spread_deg` | 23.5 (one value a run) | | | <= 37.5 | OK |

`python probe.py` exits **0**. The four old bars did not regress - each gained: dropping the moon's blue widens
the distance from the blue-dominant grass, so `t1_changed_frac` went 0.25-0.28 -> 0.35-0.38 and
`o240_hue_dist_deg` 122-138 -> 150-155. `side_face_px` rose too, so the darker grey shade did not thin the bank.

## The honest margin

s2 reads 32.16 against the 33.5 bar: 1.3 deg of room. The predicted landing was 2.9 / 5.8 / 5.5, from rotating
each pixel's card colour to the paint's hue at the same luma. The gap between 5.5 and 32.16 is the fog and mist
(`ApplyMist`, `ApplyFieldFog`, `MixFog`), which sit after the light bands and which no book knob reaches. That
is the measured remaining share of the blue excess, and it is the next lever if the owner wants the number
smaller still. No bar was widened to absorb it.

## The stills are this build's, both passes

The gas pass and the no-gas plate pass were both shot on commit `e17797daa7`, batch mode with the GPU, three
seeds, three bands - 36 JPGs and their sidecars. No file is copied from an earlier folder.

## What is here

| File | What it is |
|---|---|
| `probe.py` | unchanged from last round: same measure, same seven bars |
| `probe.json` | this run's readings, bars and max_bars |
| `gas-t1-s*`, `gas-o240-s*`, `gas-side-s*` | three seeds, three bands, each with its `-plate` and `.json` sidecar |
| `manifest.json`, `manifest-plate.json` | one row per still |

`python probe.py --selftest` exits 0 and prints the hand-worked 52.25 deg: on the fixture blob (card core,
alpha-0.25 ring, grass plate) the whole-blob mean hue is 119.75 deg, 52.25 from the paint, so the fixture still
MISSES the 33.5 bar - a blended blob must, or the bar would catch nothing.

## Guard test

`Tests/Show/GasBankNightPaintTests.TheMoonDoesNotLightTheGasBank`: the night key never multiplies the GasBank
row's colour. Its expected values come from a measurement on the committed s0 still (the card's drawn colour
(71.6, 87.2, 61.1) against the paint's (81, 90, 23) at that luma), not from the code.
