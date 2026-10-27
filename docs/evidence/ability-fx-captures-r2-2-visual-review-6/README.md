# ability-fx-captures-r2-2: visual review (6)

The owner's answer on the Decide page to "ability-fx-captures-r2-2: chain of fix units 9 deep, 26 units" was
"show me visual results". This folder changes no production file and shoots nothing new: it tiles the captures
already committed on this lane (`394d617b90`, `0c1d8bcf9f`, and the isolation cards from `2956ad4853`) into four
labelled contact sheets so the owner can look at the gas bank instead of reading its numbers.

## The sheets

- **`sheet-t1.jpg`** — the game camera, three runs, no-gas plate next to gas. Look for one continuous bank along
  the trench, not separate stripes. Captions carry each run's `bank_paint_share`.
- **`sheet-o240.jpg`** — the overview camera, same three runs, same plate/gas layout. Captions carry
  `o240_area_frac` (how much of the overview the bank covers) and `bank_width_m`.
- **`sheet-cards.jpg`** — the four isolation stills the probe tests against: plate, the accepted gas card, and
  the two rejected cards (smoke, puff) that the probe's green-lean check must fail.
- **`sheet-before-after.jpg`** — the one sheet that shows the fix instead of stating it: the rejected pale
  round's run1 at the same two poses (t1=557, o240=249) on top, this build's run1 on the bottom. Captions carry
  `bank_paint_share`, `bank_aspect` and `bank_patch_count` for both.

## The numbers against today's bars

Current bars (`docs/evidence/ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g2-g2/probe.json`):
`bank_paint_share` floor 0.175, `bank_aspect` <= 4.891, `bank_patch_count` <= 10, `gas_green_share` 0.183,
`o240_area_frac` 0.014, `bank_low_share` 0.15.

| run | `bank_paint_share` | `bank_aspect` | `bank_patch_count` | `o240_area_frac` |
|---|---|---|---|---|
| run1 | 0.2037 | 3.452 | 5 | 0.0171 |
| run2 | 0.1886 | 2.014 | 7 | 0.0193 |
| run3 | 0.2152 | 2.606 | 4 | 0.0195 |

All three runs clear every bar. The pale round's run1 (`bank_paint_share` 0.1855, `bank_aspect` 4.918,
`bank_patch_count` 10) is what the current `bank_paint_share` floor (0.175, with a half-spread waiver) was raised
to fail — that is the before/after sheet's bottom-vs-top comparison.

## Still open

The gas bank is still a sheet along the trench, not a puff. The puff brief
(`2026-10-21-gas-bank-turn-the-sheet-into-a-puff`, `docs/reference/decisions.md`) is still open and unanswered.
This lane took the buildable default consistent with that decision row: one mass instead of stripes, less pale —
which is what these pictures show, not a different concept (lumps, clots, or a dark-cored roll).

## Check

`python sheet.py --verify` — rebuilds each sheet into a temp dir, byte-compares it against the committed jpg, and
recomputes every source's sha1 against `sheet-manifest.json`. It proves the sheets are those captures unaltered;
it measures nothing and re-derives nothing.
