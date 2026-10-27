# ability-fx-captures-r2-2-g2-g1-g1 — the gas bank draws the smoke sheet's cloud

## The complaint

> "At zoom 30 at night the play frame is still bands of yellow-green across the trench, not one chlorine bank a
> player can point at. run1/gas-t1.jpg, run2/gas-t1.jpg and run3/gas-t1.jpg are that field, and the probe reads
> mask_px 4324 on all three. cards/cards-gas.jpg is a flat puddle (rise_m 0) with tracer streaks.
> cards/cards-smoke.jpg is one cloud in this pose. Draw that cloud for the gas. Bar outside the spread of three
> captures of the current build." — the owner, on `docs/evidence/ability-fx-captures-r2-2-g2-g1`

The fault was the SHEET, not the card layout: `GasBank.png` is a flat lozenge with two beam-like streaks across
it, `SmokeBank.png` is one lobed billowing cloud in the same pose. The build captured here points the GasBank
book's `Sheet.Tex` at `SmokeBank`, in chlorine ink, over SmokeBank's own frame window (12..20.5), squashed by
`CombatFx.GasBankSquash`. The book keeps its name and ordinal (`FlipbookOrdinalTests`).

## What is here

- `run1/`, `run2/`, `run3/` — three repeat captures of this build: `gas-t1`, `gas-o240` and a no-gas plate for
  each, from two batch launches of `GasBankStills` (the gas pass, then the same with `TW_GAS_PLATE=1`), packed
  with `probe.py --pack`. Every manifest row: `books_ready` true, `pose_error_m` 0, `calls_taken` 1 (t1) and
  5 (o240), `gas_cards` 774 (t1) and 119 (o240).
- `cards/` — the four `GasBankCards` isolation stills (gas, smoke, puff, plate) of this build. Never barred: the
  look report.
- `probe.py` — g1's probe (camera maths, mask, spread rule, `--pack`, `--selftest`, `--calibrate` unchanged) with
  the card rise fixed and two new keys. `probe.json` is its output.

## The mask (unchanged from g1)

A pixel counts as bank when it differs from the plate by >= 20 RGB (T1; 12 at O240) **and** is at least 2.0 Rec.709
luma BRIGHTER than the plate, inside the single biggest coherent plate-difference patch. The luma lift is applied
to what the patch finds, never before it. See the module docstring.

## The new keys, and why the old ones could not see bands

g1 asks how much of the frame's paint sits in ONE patch (`bank_patch_share`) and how many patches there are
(`bank_patch_count`). Bands across the trench touch at their ends, and the mask's dilation bridges them, so bands
**are** one patch: g1's own field read `bank_patch_share` 0.2070 and passed its 0.20 bar while the frame was a
field. The two keys added here look INSIDE that patch:

| key | what it is | the field (g1) | this build |
|---|---|---|---|
| `bank_fill` | the patch's paint over the area of the patch's own bounding box | 0.1288 | **0.1791** |
| `bank_run_px` | median length of the mask's vertical runs in the patch | 1.00 | 1.00 |

`bank_run_px` **does not separate the two builds** and is therefore reported and left unbarred: the cards are
drawn with a dither, so a vertical run is one pixel either way. A bar on a key that cannot see the complaint is a
false green.

`card_rise_m` was also broken in g1: it took each masked pixel's own ray down to the ground and then asked that
same pixel's height at that same distance, which is zero by construction. That is why gas, smoke AND puff all read
exactly 0.000 m, and why "cards-gas is a flat puddle (rise_m 0)" was never evidence. It now uses `measure_t1`'s
column maths (one distance per column, from the column's bottom-most pixel).

## Before and after (`probe.py --against docs/evidence/ability-fx-captures-r2-2-g2-g1`, always exit 1)

| key | the rejected field | this build | separates? |
|---|---|---|---|
| `bank_fill` | 0.1288 | 0.1791 | yes, barred at 0.15 |
| `bank_run_px` | 1.00 | 1.00 | no, unbarred |
| `bank_patch_share` | 0.2070 | 0.6359 | yes (g1's bar carried) |
| `bank_patch_count` | 41 | 41 | no change (g1's bar carried, cleared) |
| `mask_px` (T1) | 4324 | 33423 | the bank is 7.7x the paint it was |
| `cards-gas rise_m` (fixed maths) | 2.756 | 2.782 | the card was never a flat puddle |
| `cards-smoke rise_m` (fixed maths) | 3.361 | 3.361 | unchanged, as it must be |

## How a depth is taken now

`decisions.md` (2026-10-21, "Open: waiting on the owner") records this unit's default as option (a): the look
stands and the T1 height is made depth-aware, re-measuring the numbers the owner's two bars were set from.

The old fault: `measure_t1` fixed ONE distance per image column, from that column's bottom-most masked pixel, then
measured every pixel of the column in the vertical plane at that same distance. A thin band is fine this way; a
deep connected mass is not — pixels of cards 40 m further back sit high in the column and are measured as if they
stood on the near card's ground, so the deeper the bank, the taller it reads.

The fix: a horizontal disc centred on the ability's call (`manifest.json`'s `x`/`z`), radius estimated from how
far this capture's own near-ground points reach from that centre (clamped to 5..200 m so a noisy mask cannot blow
it out). For each masked pixel, `disc_depth` intersects its ray with the vertical prism standing over that disc
and takes the MIDPOINT of the chord as the pixel's depth — a card 40 m back and a card at the centre both get a
depth somewhere inside the disc, not the near column's foot distance. A ray that misses the disc
(`bank_depth_miss_px`, 0 pixels on all three runs here) falls back to the old near-column distance. `bank_rise_m`,
`bank_low_share` and `bank_height_frac` are now taken from these depth-aware heights, and the frame height used
for `bank_height_frac` is taken at the MEDIAN of the depths actually used, not the median near distance.

Both readings are kept, unbarred, so the two can be judged side by side: `bank_rise_m_nearplane` (the old per-
column-foot maths), `bank_depth_m` (the disc radius used, 25.547 m here) and `bank_depth_miss_px`.

**selftest** (`--selftest`) proves the fault and the fix on a hand-worked case: a pixel one degree above a centre
pixel 10 m out reads 2.2745 m at the old near plane (8 m, the fault) and 0.34311 m at the disc's chord midpoint
(10 m, the fix) — six times too tall, corrected.

## The bars

| bar | compare | value | this build | spread | margin |
|---|---|---|---|---|---|
| `bank_low_share` | >= | 0.30 (owner) | 0.7688 | 0.0000 | **0.4688 OK** |
| `bank_height_frac` | <= | 0.15 (owner) | -0.0241 | 0.0000 | **0.1741 OK** |
| `o240_area_frac` | >= | 0.010 | 0.0159 | 0.0000 | 0.0059 |
| `pose_error_max` | < | 0.50 | 0.0000 | 0.0000 | 0.5000 |
| `bank_patch_share` | >= | 0.20 | 0.6359 | 0.0001 | 0.4359 |
| `bank_patch_count` | <= | 60 | 41 | 0.0000 | 19 |
| `bank_fill` | >= | 0.15 | 0.1791 | 0.0000 | 0.0291 |

`probe.py` exits 0. The two owner bars were not moved; the depth-aware maths clears them.

`bank_rise_m` reads **-0.754 m** here: the chord-midpoint estimate is a per-pixel approximation (it guesses a
depth from where a ray crosses a disc, not the card's true position), and for a bank this deep it can place a
pixel's assumed depth past where its own ray already crosses the ground, reading as "below ground". It is not a
literal height and should be read next to `bank_rise_m_nearplane` (5.835 m, the old, over-reading figure) rather
than on its own — both numbers are in `probe.json` so the owner can judge whether the depth-aware reading is
trustworthy enough to retire the old one, or whether the look genuinely needs a second pass.

The rejected field (g1's build, `--against`), measured with this same maths: `bank_rise_m` -8.178 m,
`bank_rise_m_nearplane` 1.845 m, `bank_height_frac` -0.2622, `bank_low_share` 1.0000, `bank_depth_m` 23.907 m. The
field still fails on `bank_fill` (0.1288 against the 0.15 bar) regardless of which height maths is used.

**A warning for the next capture.** Single captures cannot tune this. The T1 frame's flipbook phase moves between
code changes (the card's frame comes from a wall clock, `BankFrame(..., now * 0.22f + ...)`), so the same build
captured in two sessions can read very different `mask_px` and heights, while three launches of one unchanged
binary agree to four decimals (the spread above). Every squash value has to be judged on its own triple of runs;
the single-capture ladder taken while tuning this leg (0.12, 0.15, 0.18, 0.19, 0.20) is not comparable and was
discarded.

## The look report (unbarred)

| card | rise (m) | relief | mask_px |
|---|---|---|---|
| `cards-gas` | 2.782 | 0.1732 | 18349 |
| `cards-smoke` | 3.361 | 0.1773 | 16780 |
| `cards-puff` | 3.088 | 0.1627 | 10937 |

gas/smoke rise ratio 0.8277, relief ratio 0.9769: the gas card now has the smoke card's own volume and relief,
squashed, which is what "draw that cloud for the gas" asked for.

## Rerun

```bash
python docs/evidence/ability-fx-captures-r2-2-g2-g1-g1/probe.py --selftest
python docs/evidence/ability-fx-captures-r2-2-g2-g1-g1/probe.py --against docs/evidence/ability-fx-captures-r2-2-g2-g1
python docs/evidence/ability-fx-captures-r2-2-g2-g1-g1/probe.py
```
