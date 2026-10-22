# ability-fx-captures-r2-2-g2-g2-g2-g1-g2: cards-gaspuff stands, and the look report gets bars (part 1 of 2), 2026-10-22

**Part 1 of 2.** This leg stands up the folder only: `probe.py` and `run1/`, `run2`, `run3` are copied in from
`ability-fx-captures-r2-2-g2-g2-g2-g1/` unchanged. The field draw (`CombatFx`) is NOT edited in this unit, so these
three runs are captures of the CURRENT build (the same build -g1 shot, before the puff card is redrawn and before
cards-gaspuff is barred). Part 2 shoots the fresh `cards1/`, `cards2/`, `cards3/` isolation sets at three frames,
bars `cards_gaspuff_rise_m`, `cards_gaspuff_aspect` and the gaspuff/smoke relief ratio, and updates this README's
"the owner's complaint" and "the bars" sections with that work.

## The owner's complaint

> cards-gaspuff is a flat green puddle and the look report is unbarred, so a puddle still passes. Draw that card
> as one standing puff with a rim, like cards-smoke, and bar it. Start from
> `lane/show/ability-fx-captures-r2-2-g2-g2-g2-g1`'s tip. Bar outside the spread of three captures of the current
> build.

## What is here

- `probe.py` — carried from `-g1` unchanged except the docstring's WHY, which now names this leg's goal and marks
  the card-standing and card-bar work as part 2's, not done yet. The camera maths, the green mask, `largest_blob`,
  the O240 clouds, `--pack`, `--calibrate`, `--selftest`, the spread check and `BAR_VALUES` are untouched.
- `run1/`, `run2/`, `run3/` — copied from `-g1` unchanged: the same four `GasBankStills` captures at
  `TW_GAS_WAIT_FRAMES` 200 / 215 / 230 (T1 frames 804 / 819 / 834, O240 frames 341 / 356 / 371), captures of the
  current build, not fresh ones. The nine carried bars (`green_low_share`, `green_height_frac`,
  `green_height_floor`, `green_area_frac`, `green_rise_m`, `o240_green_area_frac`, `o240_cloud_count`,
  `o240_cloud_share`, `pose_error_max`) clear on them exactly as they did in `-g1`.
- No `cards/` yet — the puff card's own standing and its bars are part 2's work.

## The bars

Unchanged from `-g1`; see that folder's README for the table and the spread arithmetic. This leg adds none.
