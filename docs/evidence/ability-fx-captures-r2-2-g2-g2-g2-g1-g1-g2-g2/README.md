# ability-fx-captures-r2-2-g2-g2-g2-g1-g1-g2-g2: one close cloud with a soft edge, 2026-10-22

**Part 1 of 2 is here: the probe only.** `python probe.py` returns 1 and prints "no bar set yet" for the three
new keys, by design — part 2 shoots this build's own three captures, moves the drawing, and sets those bars from
that spread. No captures live in this folder yet; part 1 measures the parent's where they lie.

## The owner's complaint

> At the close camera the gas is still separate flat green tree-cards with hard edges, and the gas card is one
> flat puddle with rise 0. Draw one cloud with a soft edge and a height a player can see. The area floor 0.0135
> sits 0.00006 under three readings that only span 0.00003, so move that floor outside this build. Start from
> `lane/show/ability-fx-captures-r2-2-g2-g2-g2-g1-g1-g2`'s tip. Bar outside the spread of three captures of the
> current build.

## Why the card rise read 0 — it was the probe, not the drawing

`-g1-g1-g2`'s `card_rise_m` took each masked pixel's **own** ground-crossing distance `d`, then asked
`height_at(..., d)` for that same pixel. Walking the same ray to the same point and subtracting the ground is
`cam_y + (ground - cam_y) - ground`, which is **0.0 for every pixel of every image**. That is why the parent's
`probe.json` reads `rise_m` 0.0 for all four cards — gas, smoke, puff and gaspuff alike. A measure that returns
the same number for every input measures nothing.

`measure_t1` never had the bug: it takes the **bottom-most** masked pixel of each pixel column, uses that one
pixel's ground distance, and measures the whole column in that single vertical plane (p45 per column, median
across columns). This unit pulls that maths into one shared `plane_heights` helper and points both callers at it,
so the two can never drift again. `selftest` case (b) pins the identity that caused the bug.

The parent's four cards, re-measured with the fix (`--measure`, unbarred as always):

| card | rise_m before | rise_m now | relief |
|---|---|---|---|
| `cards-gas` | 0.0 | 0.655 | 0.0839 |
| `cards-smoke` | 0.0 | 0.217 | 0.0888 |
| `cards-puff` | 0.0 | 0.216 | 0.1138 |
| `cards-gaspuff` | 0.0 | 5.163 | 0.1004 |

So the owner's "the gas card is one flat puddle with rise 0" is two things: the close standing puff does rise
(5.163 m), and the merged-out gas card is a 0.655 m puddle next to a 0.217 m smoke card — a third as tall as the
reference the same pose can already draw.

## The refactor changed no T1 number

`python probe.py --measure ../ability-fx-captures-r2-2-g2-g2-g2-g1-g1-g2/runN` reproduces the parent's own
published row for each of its three captures, to the digit:

| run | `green_low_share` | `green_height_frac` | `green_rise_m` | `green_area_frac` |
|---|---|---|---|---|
| run1 | 0.3462 | 0.0950 | 2.575 | 0.01359 |
| run2 | 0.3458 | 0.0933 | 2.529 | 0.01358 |
| run3 | 0.3462 | 0.0947 | 2.568 | 0.01356 |

## The three new measures, and the OLD build's readings

All three come from the T1 green mask `measure_t1` already builds (`green_shape`), so they cost no new capture.

- **`t1_cloud_count`** — components of the green mask **before** `largest_blob`, those over `CLOUD_MIN_PX` (50),
  counted exactly as `measure_o240` counts its clouds. Separate cards read many; one cloud reads one.
- **`t1_top_share`** — the biggest component's pixels over `green_px`. One cloud holds nearly all the green.
- **`green_soft_share`** — over the blob, `g` is each pixel's greenness above the plate (the quantity `greener`
  thresholds at `GREEN_BAR` 8); with `p90` its 90th percentile, this is the share of those pixels under
  `SOFT_FRAC` (0.35) `* p90`. A hard alpha cut leaves a 1-2 px step, so the share is small; a soft edge leaves a
  wide faint band.

The old build (the parent's three captures, measured here) — **the numbers part 2 must beat**:

| key | run1 | run2 | run3 | spread | what the owner's cloud should read |
|---|---|---|---|---|---|
| `t1_cloud_count` | 39 | 44 | 42 | 5 | far fewer: "separate flat green tree-cards" is 39-44 of them |
| `t1_top_share` | 0.0803 | 0.0800 | 0.0797 | 0.0006 | far higher: 8 % of the green in the biggest patch is not one cloud |
| `green_soft_share` | 0.1230 | 0.1181 | 0.1182 | 0.0049 | higher: a soft edge widens the faint band |

`t1_cloud_count` is barred `le` (a ceiling: one cloud, not forty), `t1_top_share` and `green_soft_share` `ge`.
The bar values are **deliberately absent** from `BAR_VALUES`; part 2 sets each from its own three captures, one
spread outside them, or — if the change does not move `green_soft_share` out of its own noise — leaves that one
unbarred and says so here. Never bar a measure the change did not move.

## What is here

- `probe.py` — `-g1-g1-g2`'s probe carried whole, with the four changes in its docstring: the `card_rise_m` fix
  through `plane_heights`, `green_shape`'s three measures, `--measure DIR`, and the extended `selftest`.
- `README.md` — this file.
- Part 2 adds `run1/`, `run2/`, `run3/`, `cards/` and the production change.

## Commands

```bash
cd docs/evidence/ability-fx-captures-r2-2-g2-g2-g2-g1-g1-g2-g2
python probe.py --selftest                                            # the five hand cases, exit 0
python probe.py --measure ../ability-fx-captures-r2-2-g2-g2-g2-g1-g1-g2/run1     # one directory, every key
python probe.py --measure ../ability-fx-captures-r2-2-g2-g2-g2-g1-g1-g2/cards    # the card rises
python probe.py                                                       # part 1: 1, "no bar set yet"
```
