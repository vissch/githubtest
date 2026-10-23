# The chlorine bank, round 3-g2-g2-g2-g2: the drawing changed, and the owner's own number barely moved

The owner (2026-10-23): "Make the pixels one hue. The blob hue I named is still 126.5, 106.2 and 148.8. The pass
reports an unmixed 80.0, 80.9 and 80.0, and the bar of 18.5 is that max plus 5."

Two complaints, both fair. The last three rounds each changed the MEASURE and left the drawing alone, so the
number he named never moved; and the one new bar (`o240_hue_off_deg <= 18.5`) was worked out from the very
reading it bars. This round does the opposite on both counts, and **the result is a red probe, reported as red**.

## What changed in the drawing

`_Solid` on the `GasBank` book (`Flipbook_URP.shader`, `FlipbookFx.cs`, commit `235f4414ab`): the card's soft
alpha shoulder is pulled up, `alpha = saturate(alpha / (1 - _Solid))`, so a blob pixel should be the card's own
colour instead of `a*card + (1-a)*grass`. Monotone, and 0 stays 0, so the silhouette keeps its ragged shape.

## What it did to the owner's number: almost nothing

| `o240_blob_hue_deg` | s0 | s1 | s2 | spread |
|---|---|---|---|---|
| before (`d797cb6c6e`, `Solid` absent) | 126.5 | 106.2 | 148.8 | 42.6 |
| `Solid = 0.6` (the stills here) | 123.3 | 104.9 | 143.3 | 38.4 |
| `Solid = 0.9` (shot too, not kept) | 121.8 | 104.9 | 140.7 | 35.8 |

Three to five degrees from the first step, one and a half more from half again as much alpha, and at 0.9 the
T1 bank went to opaque green slabs - the critic's "moss". The alpha is not the lever this reading needs, so the
stills kept here are the 0.6 build, which is what is committed.

## Why the alpha cannot be the lever (the reading the stills themselves give)

`o240_bank_hue_deg` is the hue of the blob's CORE - the top quartile of plate difference, the most opaque
pixels in the frame. It reads **88.7 / 85.7 / 98.2 deg**, i.e. 18-31 deg off the paint's own 67.5, and `_Solid`
does not move it either. A perfectly opaque bank would therefore land near 88 deg, not near 67.5: what sits
between the paint and the pixel is **the night light on the drawing's own darks**, not a card-over-grass blend.
`o240_unmix_hue_deg` (the shot-minus-plate direction, alpha-free) agrees: 79.5 / 80.0 / 79.4 deg, unmoved by
`_Solid`, as an alpha-free reading must be. The whole-blob mean is then dragged further by the blob's faint
outer pixels, whose raw colour is mostly the blue-dominant grass behind them.

So making the owner's number small needs the bank's COLOUR under the night rig changed - `Mood`, `Lit`,
`Low`/`High`, or the biome tint in `CombatFx.ApplyTints` - not more alpha. That is a second change of look and
outside this plan, which named `_Solid` as its lever and its only fallback.

## The two new bars, and why they stay where they are

Both read `o240_blob_hue_deg`, the owner's own number, untouched. Both are derived from three captures of the
build as it stood (`d797cb6c6e`), never from the number they bar:

126.5 / 106.2 / 148.8 -> off from the paint **59.0 / 38.7 / 81.3**, min 38.7, spread **42.6**.

| Bar | Rule | Value |
|---|---|---|
| `o240_blob_off_deg` (per seed) | `min(seeds) - 5.0`, rounded DOWN to 0.5: `38.7 - 5 = 33.7` | **<= 33.5** |
| `o240_blob_spread_deg` (once a run) | `spread - 5.0`, rounded DOWN to 0.5: `42.6 - 5 = 37.6` | **<= 37.5** |

Rounded DOWN, the mirror of the `>=` bars' round-up, so rounding can never pull an upper bound back inside the
spread it must sit outside of. **Neither was moved to make this run green**, and the run is not green.

## This run

| Bar | s0 | s1 | s2 | Want | |
|---|---|---|---|---|---|
| `t1_changed_frac` | 0.2474 | 0.2699 | 0.2808 | >= 0.08 | OK |
| `texture_var_drop` | 0.5622 | 0.6988 | 0.6054 | >= 0.40 | OK |
| `o240_hue_dist_deg` | 134.16 | 137.67 | 122.62 | >= 103.5 | OK |
| `side_face_px` | 136295 | 50879 | 201780 | >= 41000 | OK |
| `o240_hue_off_deg` | 12.03 | 12.50 | 11.91 | <= 18.5 | OK |
| `o240_blob_off_deg` | 55.77 | 37.44 | 75.84 | <= 33.5 | **MISSED** |
| `o240_blob_spread_deg` | 38.4 (one value a run) | | | <= 37.5 | **MISSED** |

`python probe.py` exits **1**. The four old bars and `o240_hue_off_deg` still pass, so nothing regressed; the
two bars on the owner's own reading are the honest open gap.

## The stills are this build's, both passes

Both the gas pass and the no-gas plate pass were shot on the same commit (`_Solid = 0.6`), batch mode, three
seeds, three bands - 36 JPGs and their sidecars. No file is copied from an earlier folder.

## What is here

| File | What it is |
|---|---|
| `probe.py` | r2-3-g2-g2-g2's measure plus the two bars above; no new measure, no old bar changed |
| `probe.json` | this run's readings, bars and max_bars |
| `gas-t1-s*`, `gas-o240-s*`, `gas-side-s*` | three seeds, three bands, each with its `-plate` and `.json` sidecar |
| `manifest.json`, `manifest-plate.json` | one row per still |

`python probe.py --selftest` exits 0 and prints the hand-worked 52.25 deg: on the fixture blob (card core,
alpha-0.25 ring, grass plate) the whole-blob mean hue is 119.75 deg, 52.25 from the paint, so the fixture MISSES
the 33.5 bar - a blended blob must, or the bar would catch nothing.

## Still open

The owner's ask itself. The bank's hue under the night light, not its alpha, is what has to move next; see
`Mood` / `Lit` / `Low`-`High` on the `GasBank` row and `CombatFx.ApplyTints`. A change of look of that size
wants a concepts brief first (the role brief's rule) - this round's `_Solid` did not, because the owner asked
for it in writing.
