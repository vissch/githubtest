# ability-fx-captures-r2-2-g2-g2-g1: one gas BODY, not stripes, 2026-10-21

**Part 1 of 2 is in: the look changed, the captures are not shot yet.** `python probe.py` only clears once
`run1/`, `run2/` and `run3/` are here; part 2 shoots them off this build and sets `BAR_VALUES` from their spread.

## Why the look changed

The owner, on `-g2-g2`'s three captures: gas-t1 run1, run2 and run3 (frames 804, 819, 834) are the same
horizontal green stripes, and `cards-gas.jpg` is a flat puddle with `rise_m` 0. Draw one gas body like
`cards-smoke.jpg`.

Read, not guessed: `FlipbookFx`'s `GasBank` row has `Ink = (0.00, 0.35)` — its drawing fills the BOTTOM THIRD of
each cell — and `CombatFx.Banks.cs` squashed the card to `GasBankSquash` 0.55. A 7 m card therefore painted a band
~1.3 m tall and 7 m wide: a horizontal stripe, one per 4 m field cell. The `SmokeBank` row is `Ink = (0.03, 0.87)`,
drawn square, which is the billowing body in `cards-smoke.jpg`.

## What part 1 did

- `FlipbookFx.Book.GasBody`, appended before `Count` with its `Sheets` row LAST: `SmokeBank`'s PNG, grid, shape and
  ink with `GasBank`'s colour, `Lit`, `Mood` and `Bank` flag. A second row on one PNG is the house pattern
  (`MineSmoke` is `WreckSmoke.png`), so no new art and `books.Ready` cannot break. `Book.GasBank` stays: deleting it
  would shift every later ordinal.
- `CombatFx.DrawGasBank` draws `Book.GasBody`, anchored at `Ground - 0.2f` (the smoke bank's anchor), `width` by
  `width * GasBodyHeight` (0.75), frames `GasBodyFirst` 12 .. `GasBodyLast` 20.5 — the old 17..28 would run off a
  22-frame book and draw nothing. `GasBankSquash` is kept as a const for the old numbers but no longer drawn with.
- `CombatFx.ApplyTints` tints `GasBody` as it tints `GasBank`: it overwrites a bank's tint every scene, so a row
  tint alone would be thrown away.
- `GasBankCards` (the isolation stills) draws the card the bank now draws, so `cards-gas.jpg` is the body.
- Tests: `KnobsTests` counts `GasBody` among the `Deep` books, `FlipbookOrdinalTests` knows it draws `SmokeBank.png`,
  and `BurstRecipeTests.TheGasBodyStandsOffTheGround_ButIsStillABank` replaces the old "under a man's head" bar:
  7 m * 0.75 * 0.87 = 4.6 m, above 2.5 m (a body, not a painted line) and below 6 m (a bank, not a column).

With `width` 5-7 m the ink tops out near 4.6 m: a body a man is swallowed by, still a bank rolling along the ground.

## What part 2 must do

`probe.py` is carried from `-g2-g2` unchanged (the measure: green the gas ADDED over the plate, the frame guard,
`largest_blob`, the camera maths). Part 2 shoots three `GasBankStills` runs of THIS build at three different
`TW_GAS_WAIT_FRAMES`, repacks `cards/` from `GasBankCards`, and resets `BAR_VALUES` from their spread.
`green_low_share >= 0.30` and `green_height_frac <= 0.15` are the owner's bars and are not relaxed: if a 4.6 m body
pushes `green_low_share` under 0.30, lower `GasBodyHeight` and reshoot.
