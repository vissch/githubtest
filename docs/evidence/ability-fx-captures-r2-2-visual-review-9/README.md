# ability-fx-captures-r2-2-visual-review-9

The owner's answer on the Decide page to "ability-fx-captures-r2-2: chain of fix units 9 deep, 26 units" was
"show me visual results". The nine fix units each shot three keeper captures and committed them under
`docs/evidence/`; nobody had put them in front of him — he had only read tables of numbers
(`green_low_share`, `t1_visible_clouds`, ...). These sheets tile the captures already on disk, in the order the
chain made them, so he can look instead of read.

## Sheets

- **`sheet-t1.png`** — every unit's `run1/gas-t1.jpg` (the close camera, where the "one cloud or a man's-head
  pool" question is argued), oldest unit top-left, one panel per unit. `t1_visible_clouds` (or `t1_cloud_count`
  where the newer key does not exist yet) and `green_low_share` from that unit's `probe.json` are printed under
  each panel; a dash where the unit's schema does not carry that number. 9 panels: the newest unit,
  `ability-fx-captures-r2-2-g2-g2-g2-g1-g1-g2-g2-g1-g1`, had shot no keeper captures (its skirt never earned a
  bar), so its panel is a fresh `GasBankStills` gas + plate pass of HEAD as this leg found it, in `run-today/`
  rather than `run1/`, labelled "HEAD not landed": it carries `GasPuffSkirtWide` 2.2 / `GasPuffSkirtStand` 0.07,
  which breaks the owner's 0.30 `green_low_share` floor and was never landed, so this panel is not the look of
  the landed bank — only the closest look anyone has put in front of him yet.
- **`sheet-o240.png`** — the same units' `run1/gas-o240.jpg` (the far camera).
- **`sheet-cards.png`** — the five isolation stills (gas, gaspuff, puff, smoke, plate) of the newest unit that
  has a `cards/` folder, `ability-fx-captures-r2-2-g2-g2-g2-g1-g1-g2-g2-g1`.

Built with:
```
python docs/evidence/ability-fx-captures-r2-2-visual-review-9/sheet.py
```

## The one-line reading

Across the chain the bank went from invisible to the capture path (closed 2026-10-19, `decisions.md`) to about
24 soft clouds at the close camera — and stopped there, held by the owner's own `green_low_share >= 0.30` floor:
every width that merges the cards into one mass pulls `low_share` under 0.30, and every density that merges them
tight enough to read as one cloud pushes `low_share` to 0.07-0.11. The sheets show the same 23-24 clouds the
numbers already said; the open question ("The close gas: one cloud, or gas that still pools under a man's
head?", `decisions.md`) is unchanged by seeing it.

Brief on the Decide page: "The close gas, seen".
