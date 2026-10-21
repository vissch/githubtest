# ability-fx-captures-r2-2-g2-g2-g1: one gas BODY, not stripes, 2026-10-21

**Part 2 is in: the body is shot, and `python probe.py` exits 1 on two owner bars** (`green_low_share`,
`green_height_frac`). That is not a bug to work around: it is the open question in `docs/reference/decisions.md`
("The gas body against the flat-bank bars"). Nothing was relaxed in the owner's name.

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

## Part 2: the three captures (this build, GasBodyHeight 0.75)

Shot from the repo root with the editor closed, each run a gas pass and an identical plate pass:
`TW_STILLS_DIR=<tmp>/runN TW_GAS_WAIT_FRAMES=200|215|230 Unity.exe -batchmode -projectPath <checkout>/trench-warfare-3d
-runTests -testPlatform EditMode -testFilter GasBankStills`, then the same with `TW_GAS_PLATE=1`; `cards/` from one
`-testFilter GasBankCards` launch. All seven launches exited 0.

| run | wait frames | gas-t1 frame | o240 frame | gas cards drawn |
|---|---|---|---|---|
| run1 | 200 | 804 | 341 | 774 |
| run2 | 215 | 819 | 356 | 769 |
| run3 | 230 | 834 | 371 | 763 |

Three different frames, so the probe's same-frame refusal is clear.

## The bars

| measure | runs | spread | bar | margin | why |
|---|---|---|---|---|---|
| `green_body_share` | 0.9078 / 0.9548 / 0.9025 | 0.0523 | ge 0.80 | 0.1025 | one blob holds the green; the stripes read 0.11 |
| `green_relief` | 0.2065 / 0.2184 / 0.2061 | 0.0123 | ge 0.09 | 0.1161 | the owner's anchor: at least half the smoke card's relief 0.1933 |
| `green_rise_m` | 8.775 / 9.509 / 8.618 | 0.891 | ge 7.0 | 1.618 | the barred version of the number that read 0.359 on the stripes |
| `green_area_frac` | 0.5338 / 0.5585 / 0.5257 | 0.0328 | ge 0.45 | 0.0757 | the round number under min - spread |
| `o240_green_area_frac` | 0.0268 / 0.0272 / 0.0287 | 0.0019 | ge 0.020 | 0.0068 | the round number under min - spread |
| `pose_error_max` | 0.0000 x3 | 0 | lt 0.50 | 0.50 | `GasBankStills.PoseBar` |
| `green_low_share` | 0.0968 / 0.0881 / 0.0997 | 0.0116 | ge 0.30 | **-0.2119 FAILED** | the owner's bar from the FLAT bank |
| `green_height_frac` | 0.3913 / 0.4453 / 0.3826 | 0.0627 | le 0.15 | **-0.2953 FAILED** | the owner's bar from the FLAT bank |

## Why the two bars fail, and what was tried

`green_low_share >= 0.30` ("most of the green is under a man's head") and `green_height_frac <= 0.15` ("the bank is a
low band in the frame") were set on the flat bank the owner has now asked to replace. A body that stands 4.6 m cannot
clear them. `GasBodyHeight` was lowered to 0.6 and all three runs reshot: `green_low_share` moved to
0.1331 / 0.1354 / 0.1400 and `green_height_frac` to 0.2694 / 0.2749 / 0.2696 - still far outside, and a smaller body
is the old stripe by another name. The 0.6 build was reverted; the captures here are the committed 0.75 build.

The T1 stills also show the body repeating once per 4 m cell: a carpet of green pads rather than one rolling cloud.
That is a look question for the owner, named in the decisions row, not a bar.

## The cards picture pair

`cards/cards-gas.jpg` is now the body card beside `cards/cards-smoke.jpg`: relief 0.1218 against the smoke card's
0.2178 (gas/smoke relief ratio 0.5592), mask 22033 px against 1401 px - the gas card is the big billowing shape it
was asked to be, not the flat puddle. `rise_m` is 0.000 for all three cards because the cards pose's single card sits
on the ground plane the maths measures from; it is a look report, barred on nothing.
