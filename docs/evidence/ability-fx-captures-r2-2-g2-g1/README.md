# ability-fx-captures-r2-2-g2-g1: one gas bank, and a measure that can see a zebra, 2026-10-20

**`python probe.py` clears, exit 0.** The build it bars is the gas bank's rewrite in
`Presentation/Camera/CombatFx.Banks.cs` (commit `5c0c33620a`, part 1 of this lane): wide overlapping cards at low
alpha, with the cell hash breaking the top line, the brightness and the roll.

## The owner's complaint

> At zoom 30 at night the play frame is a zebra of flat yellow-green cards, not a chlorine bank. `run1/gas-t1.jpg`,
> `run2/gas-t1.jpg` and `run3/gas-t1.jpg` are that zebra, and they are all frame 804. `run1/gas-o240.jpg` is the
> same paint from zoom 240, frame 341. `cards/cards-gas.jpg` is a flat green puddle with tracer streaks.
> `cards/cards-smoke.jpg` is one cloud in this pose. Draw one bank a player can point at. Bar outside the spread
> of three captures of the current build.

The zebra was one card per 4 m gas cell, 5-7 m wide, squashed flat, at alpha 0.6-1.0, every long axis horizontal.
The cards barely overlapped, so each card's own hard silhouette stayed visible: stripes over the whole field.
Every bar g2 set (the bank lies low, it is not tall, it covers area at O240, the pose is right) is true of a zebra
too, so **none of g2's bars could see the complaint**. That is what this folder fixes on the measuring side.

## What is here

- `run1/`, `run2/`, `run3/` — three runs of the four `GasBankStills` captures (`gas-t1`, `gas-t1-plate`,
  `gas-o240`, `gas-o240-plate`) plus `manifest.json`/`manifest-plate.json`, each from its own pair of batch
  launches (gas pass, then the same with `TW_GAS_PLATE=1`) of the part-1 build. Every manifest row has
  `books_ready: true`, `gas_cards` 774 (t1) / 119 (o240) on the gas pass, `calls_taken` 1 (t1) / 5 (o240) and
  `pose_error_m` 0. `draw_calls` is 133 (t1) and 118 (o240) — **exactly the zebra's**, so the wider cards cost
  nothing: the card count did not change.
- `cards/` — the four `GasBankCards` isolation stills, reshot from this build, for the unbarred look report.
- `probe.py` — g2's probe (camera maths, mask, spread rule, `--pack`, `--selftest`, `--calibrate` all unchanged)
  plus `blobs()`, the two zebra keys, and `--against DIR`.
- `probe.json` — all three runs' numbers, the look report and `BAR_VALUES`.

## The mask

Unchanged from g2: a gas pixel must differ from the plate by >= 20 RGB (T1) or >= 12 RGB (O240), sit inside the
single largest coherent plate-difference patch (T1 only), and be brighter than the plate (Rec.709 luma >= 2).
The fault being fixed here was never the mask; it was that nothing counted the patches.

## The zebra test (new)

Two keys, both from the T1 plate difference the mask already computes:

- `bank_patch_share` = `mask_px_patch / mask_px_raw` — how much of the frame's plate difference sits in the ONE
  biggest patch. One bank: most of the paint is in one place. A zebra: the paint is spread over separate cards.
- `bank_patch_count` = how many plate-difference patches hold 200 px or more (`PATCH_MIN_PX`). One bank is few
  patches; a zebra is many.

`largest_blob` is now `blobs(mask, w, h, dilate)` keeping the biggest component, so the existing callers and
`--selftest` did not move.

**Before and after** (`python probe.py --against docs/evidence/ability-fx-captures-r2-2-g2`, which measures g2's
own three runs with THIS probe and always exits 1):

| build | `bank_patch_share` | `bank_patch_count` |
|---|---|---|
| the zebra (g2, `843a03570c`) | 0.1863 | 90 |
| this build (part 1, `5c0c33620a`) | 0.2070 | 41 |

The patch count more than halves: the field is no longer tiled with separate card silhouettes. The share moves
less (the cards are thinner now, so faint paint reaches further cells and keeps `mask_px_raw` high), which is why
both keys are barred, not just one.

## The bar: the spread rule

Unchanged from g2. Three runs must all sit on the pass side AND the nearest value must clear the bar by at least
the three runs' spread. Under three runs the probe exits 1 before looking at a bar. As in g2, the three captures
came back identical to four decimals (deterministic sim, frame-counted waits), so the spread is ~0.

## The bars

| key | cmp | bar | the three runs | spread | margin | why |
|---|---|---|---|---|---|---|
| `bank_low_share` | `ge` | 0.30 | 0.4371 x3 | 0.0000 | 0.1371 | owner-decided, head height (`HEAD_M` 1.8) |
| `bank_height_frac` | `le` | 0.15 | 0.0799 x3 | 0.0000 | 0.0701 | owner-decided, r2's bar on this test |
| `o240_area_frac` | `ge` | 0.010 | 0.0131 x3 | 0.0000 | 0.0031 | g2's bar, kept |
| `pose_error_max` | `lt` | 0.50 | 0.0000 x3 | 0.0000 | 0.5000 | `GasBankStills.PoseBar` |
| `bank_patch_share` | `ge` | 0.20 | 0.2070, 0.2071, 0.2071 | 0.0001 | 0.0070 | between the zebra's 0.1863 and this build's 0.2070 |
| `bank_patch_count` | `le` | 60 | 41 x3 | 0.0000 | 19.0000 | between the zebra's 90 and this build's 41 |

Both new bars sit between the old reading and the new one, so the zebra fails them: that is the point.

## The look report (unbarred)

From `cards/`, reshot from this build: gas relief 0.1730, smoke 0.1773, puff 0.1627; gas/smoke relief ratio
0.9757. Both rises are 0.000 m — this isolation pose holds the card at ground level, as in g2 — so the rise ratio
is not a useful number here. Nothing here is a pass bar.

## For the owner: the look

`run1/gas-t1.jpg` next to g2's `run1/gas-t1.jpg` is the honest before/after. The stripes are gone and the gas now
reads as one body in the middle of the field, but **it is fainter than the zebra was**: one card is a haze
(`GasBankThin` 0.14, `GasBankFull` 0.5) and only the overlap builds it up. Raising the alpha was tried in this
leg (0.2 / 0.65, the most the `TheGasBankStacksIntoDensity` test allows) and reverted: it made BOTH zebra numbers
worse (`bank_patch_share` 0.1845, `bank_patch_count` 51) because the extra paint spreads over far cells instead
of thickening the body. Making the bank both one mass and strongly visible needs a denser, less transparent gas
sheet, not another alpha number — a new sheet is out of this lane's scope.
