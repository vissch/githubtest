# ability-fx-captures-r2-2-visual-results-2: the chain, drawn together

The owner's answer on the Decide page to "ability-fx-captures-r2-2: chain of fix units 8 deep, 26 units" was
"show me visual results". This folder shoots nothing new: it draws the stills already committed by the nine
chain folders under `docs/evidence/ability-fx*/` into three sheets a person can look at in one place.
`python sheet.py` rebuilds them; it is read-only and needs no Unity, no editor, no capture run.

## What the three images are

- **`chain-t1.jpg`** — the nine `gas-t1.jpg` play-zoom stills (each folder's `run1/gas-t1.jpg`, or its own
  `gas-t1.jpg` where there is no `run1/`), oldest to newest, left to right then down, each captioned with its
  three headline keys (`gas_green_share`, `bank_paint_share`, `bank_patch_count`) and pass/FAIL against that
  folder's own bar. A key an older folder's `probe.json` never had prints `n/a`.
- **`chain-o240.jpg`** — the same nine folders, the `gas-o240.jpg` overview band.
- **`diff-now.jpg`** — the build as it stands today (newest folder's `run1`): the plate (gas off), the gas still
  (gas on), and `|gas - plate| x 4` computed in numpy int16 so the subtraction cannot wrap. The third panel is
  what the ability FX actually puts on screen, with the night field subtracted away — it is clearly green-lifted
  over the gas-called area.

## The chain, one line per step

1. `ability-fx-captures-r2` — the chlorine bank, 2026-10-19. The bank draws and reaches the capture; what was
   broken was the green test, which at night found exactly one pixel more green than red/blue.
2. `ability-fx-captures-r2-2-g2` — a gas measure three captures can agree on, 2026-10-20.
3. `ability-fx-captures-r2-2-g2-g1` — one gas bank, and a measure that can see a zebra, 2026-10-20.
4. `ability-fx-captures-r2-2-g2-g1-g2` — three captures on three frames, and a bar on the paint, 2026-10-21.
5. `ability-fx-captures-r2-2-g2-g1-g2-g2` — paint by direction, not hue, 2026-10-21.
6. `ability-fx-captures-r2-2-g2-g1-g2-g2-g2` — the gas test rejects smoke and the puff card, 2026-10-21.
7. `ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1` — one low chlorine bank, not a sheet of stripes, 2026-10-22.
8. `ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g2` — thin edges stop counting, and aspect gets a bar, 2026-10-22.
9. `ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g2-g1` — the ground mat, and the patch ceiling still red, 2026-10-22.

## Where the chain stands today

Quoting `docs/evidence/ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g2-g1/README.md`: **`python probe.py` exits 1
here.** Eight of nine bars pass; `bank_patch_count` reads **13 / 17 / 10** against a ceiling of **4**, and that
folder's `probe.py` still exits 1 on it. Nothing in this folder, or in that one, was loosened to make the number
look better. `chain-t1.jpg`'s last panel shows that FAIL in red for the same reason.

## Rebuild

```
python docs/evidence/ability-fx-captures-r2-2-visual-results-2/sheet.py
```
