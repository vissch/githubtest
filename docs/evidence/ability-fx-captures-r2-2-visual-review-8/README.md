# ability-fx-captures-r2-2-visual-review-8: look at the gas chain, 2026-10-27

The owner's answer to "ability-fx-captures-r2-2: chain of fix units 8 deep, 26 units" was **"show me visual
results"**. This folder is that answer: the eight rounds of the gas chain, side by side, as pictures.

**This unit changes no production file and shoots nothing new.** Every still is one each round already committed,
and every caption number is read out of that round's own `probe.json` — nothing here is typed by hand, so a sheet
cannot claim a number a round did not measure.

## The sheets

| Sheet | What to look at |
|---|---|
| `sheets/before-after.png` | **Start here.** 2 rows x 3: the first round and the last, each as the still, its no-gas plate, and the difference between them boosted x4. Night stills read almost black raw; the boosted panel is what makes the gas visible to a person. Top row: a faint speckle. Bottom row: big connected green clouds. |
| `sheets/chain-t1.png` | The close (T1) camera, one tile per round, oldest first, four across. The discs appearing, flattening, standing up and finally merging. |
| `sheets/chain-o240.png` | The same eight rounds at the overview (O240) camera, where the gas had to stop being one solid wash. |

## The eight rounds

Numbers are run1's, from each round's `probe.json` (rounds 1-2 use the older `bank_*` / `gas_t1_*` key names;
`-` is a number that round never measured).

| Round | What changed | height_frac | low_share | rise_m |
|---|---|---|---|---|
| `ability-fx-captures-r2` | the chlorine bank: it IS in the picture, `probe.py`'s green test was the fault | 0.1352 | 0.324 | - |
| `ability-fx-captures-r2-2-g2` | a gas measure three captures can agree on | 0.095 | 0.4267 | 2.149 |
| `ability-fx-captures-r2-2-g2-g2` | count the green, bar it outside three different frames | 0.0116 | 1 | 0.359 |
| `ability-fx-captures-r2-2-g2-g2-g2` | bar the rise, count O240's clouds | 0.0116 | 1 | 0.359 |
| `ability-fx-captures-r2-2-g2-g2-g2-g1` | a floor under the height, and the puff cut to fit it | 0.0768 | 0.4136 | 2.38 |
| `ability-fx-captures-r2-2-g2-g2-g2-g1-g1` | fewer, taller close puffs instead of a sheet | 0.095 | 0.3462 | 2.575 |
| `ability-fx-captures-r2-2-g2-g2-g2-g1-g1-g2` | overview clouds with gaps, a count ceiling, real digits | 0.095 | 0.3462 | 2.575 |
| `ability-fx-captures-r2-2-g2-g2-g2-g1-g1-g2-g2` | one close cloud with a soft edge | 0.0836 | 0.3045 | 2.653 |

## Still open

The puff brief of 2026-10-22 (`docs/reference/decisions.md`, the dated row) is **answered** — option (b), one
close cloud, with the `green_low_share` floor coming down to about 0.15 — but not yet built. The last round above
is still the `GasPuffWide` 2.0 look, so the bottom row of `before-after.png` is what option (b) is measured
against, not option (b) itself.

## The check

```bash
python docs/evidence/ability-fx-captures-r2-2-visual-review-8/sheet.py --verify
```

Exit 0, one `ok` line per tile: every source still is still there with the sha1 the sheet was drawn from, every
PNG opens, and each sheet's tile count matches `sheet-manifest.json`. `python sheet.py` (no argument) redraws
all three sheets and the manifest.
