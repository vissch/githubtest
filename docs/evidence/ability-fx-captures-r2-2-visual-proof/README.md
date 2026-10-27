# ability-fx-captures-r2-2-visual-proof: the chain, laid out as pictures

The owner's answer on the Decide page to "ability-fx-captures-r2-2: chain of fix units 13 deep, 26 units" was
"show me visual results". Every picture already existed, committed in the 14 `docs/evidence/ability-fx-captures-r2*`
round folders (oldest `ability-fx-captures-r2`, newest
`ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g1-g1-g2-g1-g1-g1-g1`, commit `0dc3b949cf`); nothing viewable tied them
together. `sheet.py` does only that: it lays out stills that are already in the tree, it does not shoot anything
new. No Unity, no network.

## The three sheets

- **`before-after-t1.jpg`** — the one picture that answers "show me visual results" on its own: the no-gas plate,
  round 1's `gas-t1.jpg` (striped, broken green patches), round 14's `run1/gas-t1.jpg` (one unbroken green sheet
  with a soft ragged edge). Full 1280x720 tiles, from the newest round's `run1/gas-t1-plate.jpg`, the oldest
  round's `gas-t1.jpg`, and the newest round's `run1/gas-t1.jpg`.
- **`chain-t1.jpg`** — all 14 rounds in order, 640x360 each, a no-gas plate tile first, so every lever in the chain
  is visible at the play zoom (T1) in one sheet.
- **`chain-o240.jpg`** — the same 14 rounds at the overview band (`gas-o240.jpg`), so the fixes can be checked at
  distance too: the bank did not shrink to buy the T1 look.

Each round's tile is labelled with its number, its folder suffix (the `g1`/`g2` path from the root round), its
one-line lever (from that round's commit subject), and `patches=N paint=0.NN` taken straight from that round's own
committed `probe.json` (`runs.run1.bank_patch_count` / `runs.run1.bank_paint_share`; `-` where the round predates
that key — `bank_patch_count` starts at round 3, `bank_paint_share` at round 4, round 1 has no `runs` block at
all).

## The headline progression

| | round 1 (`r2`) | round 13 | round 14 (newest) |
|---|---|---|---|
| `bank_patch_count` (run1/2/3) | n/a | 19 / 13 / 6 | 2 / 5 / 3 |
| `bank_paint_share` | n/a | 0.84-0.92 | 0.9841-0.9899 |

The chain opens with a single hand-packed card and closes with `bank_patch_count` 2/5/3 against the owner's cap of
8 (rule's bar 5+3=8, met exactly — see the newest round's README) and `bank_paint_share` 0.984-0.990, the highest
recorded in any round. To the eye (`before-after-t1.jpg`, `chain-t1.jpg`): round 1 is a single pale card; the
mid-chain rounds are visibly striped or blotched (zebra patches, dark row bands); round 14 is one unbroken green
sheet with a soft ragged edge, not a hard poster.

## What is still open

`row_gap_deepest` — the measure of the dark line between card rows — still misses the owner's cap of 0.15 in round
14 (runs 0.4473-0.5207), and the newest round's README shows why it cannot pass as the measure is defined: the
gas cloud's own plateau is already flat, the deepest row always sits at an end of the trimmed core (the cloud's
own silhouette, not a gap), and a perfectly smooth dome scores 0.30-0.34 on this very measure — twice the cap. That
round puts three options to the owner (drop the cap, change what the measure trims, or stay blocked) and
`decisions.md` carries it as an open question. This folder does not re-argue it; the pictures above show the bank
read as one sheet to the eye regardless.

## Rebuild

```
python sheet.py          # writes the three JPEGs, prints the 15 tile sources for chain-t1.jpg
python sheet.py --check  # rebuilds in memory, compares bytes against the committed JPEGs, exit 0 only if identical
```

`--check` is what makes these sheets provably the rounds' own committed stills re-laid-out, not hand-art: it
re-derives every pixel from the same source files and byte-compares against what is committed here.
